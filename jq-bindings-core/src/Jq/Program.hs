{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Jq.Program
  ( OpenProgram(..)
  , ClosedProgram(..)
  , renderClosedProgram
  , parseProgram
  , jq
  ) where

import           Control.Applicative (empty, optional)
import           Control.Monad
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import           Data.Data
import qualified Data.List.NonEmpty as NE
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec hiding (optional)
import           Text.Read (readMaybe)

-- | AST for expressions that can be further extended with additional key
-- queries, etc.
data OpenProgram
  = AtKeys (NE.NonEmpty BS8.ByteString) (Maybe OpenProgram)
  | AtIndices (NE.NonEmpty Int) (Maybe OpenProgram)
  | Iterator (Maybe OpenProgram)
    -- ^ return all elements of an array or values of an object
  deriving (Show, Eq, Data)

-- TODO array/string slice
-- TODO object construction

-- | AST for "top level" programs that cannot be extended with field accessors
-- or array indices.
data ClosedProgram
  = Identity (Maybe OpenProgram)
  | Pipe ClosedProgram ClosedProgram
  | Comma ClosedProgram ClosedProgram
  | AsArray ClosedProgram (Maybe OpenProgram)
  | Parens ClosedProgram (Maybe OpenProgram)
  | RecursiveDescent (Maybe OpenProgram)
  -- This can have certain accessors, but not regular key accessors
  deriving (Show, Eq, Data)

-- This is not a complete embedding of the jq language. The AST does not
-- include literal expressions or many other fancy things.

renderOpenProgram :: OpenProgram -> BS8.ByteString
renderOpenProgram = \case
  AtKeys keys p ->
    let renderKey k = "\"" <> k <> "\""
        keyBs = map renderKey $ NE.toList keys
     in "[" <> BS8.intercalate "," keyBs <> "]"
        <> foldMap renderOpenProgram p
  AtIndices idxs p ->
    let renderIdx = BS8.pack . show
        idxBs = map renderIdx $ NE.toList idxs
     in "[" <> BS8.intercalate "," idxBs <> "]" <> foldMap renderOpenProgram p
  Iterator p -> "[]" <> foldMap renderOpenProgram p

-- | Produce a 'ByteString' that can be fed to jq as a valid program
renderClosedProgram :: ClosedProgram -> BS8.ByteString
renderClosedProgram = \case
  Identity p -> "." <> foldMap renderOpenProgram p
  Pipe a b -> renderClosedProgram a <> " | " <> renderClosedProgram b
  Comma a b -> renderClosedProgram a <> ", " <> renderClosedProgram b
  AsArray p o -> "[" <> renderClosedProgram p <> "]" <> foldMap renderOpenProgram o
  Parens p o -> "(" <> renderClosedProgram p <> ")" <> foldMap renderOpenProgram o
  RecursiveDescent p -> ".." <> foldMap renderOpenProgram p

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser = Parsec String ()

parseProgram :: Parser ClosedProgram
parseProgram =
  parseClosed <* spaces <* eof

parseClosed :: Parser ClosedProgram
parseClosed = do
  between spaces spaces (
    choice [ parseParens
           , parseAsArray
           , parseRecursiveDescent
           , parseIdentity
           ]
    )
    `chainl1` (Comma <$ char ',')
    `chainl1` (Pipe <$ char '|')

parseIdentity :: Parser ClosedProgram
parseIdentity = do
  void $ char '.'
  Identity <$> try (optional (parseOpen True True))

parseRecursiveDescent :: Parser ClosedProgram
parseRecursiveDescent = do
  void $ try (string "..")
  RecursiveDescent <$> optional (parseOpen False False)

parseAsArray :: Parser ClosedProgram
parseAsArray = do
  inside <-
    between (char '[' *> spaces)
            (spaces <* char ']')
            parseClosed
  AsArray inside <$> optional (parseOpen False True)

parseParens :: Parser ClosedProgram
parseParens = do
  inside <-
    between (char '(' *> spaces)
            (spaces <* char ')')
            parseClosed
  Parens inside <$> optional (parseOpen False True)

parseOpen :: Bool -> Bool -> Parser OpenProgram
parseOpen isFirst allowAtKey = do
  let openBracketStart =
        choice
          [ parseAtKeys
          , parseIterator
          , parseAtIndices
          ]
  c <- choice
         [ bool empty (parseAtKey isFirst) allowAtKey
         , char '[' *> spaces *> openBracketStart
         ]
  c <$> optional (parseOpen False True)

parseAtKey :: Bool -> Parser (Maybe OpenProgram -> OpenProgram)
parseAtKey isFirst = do
  -- consume the period if this is the first in a sequence. This is to account
  -- for the Identity also introducing the '.' character
  key <- bool try id isFirst
       $ char '.' *> parseKey
  pure $ AtKeys (pure key)

parseAtKeys :: Parser (Maybe OpenProgram -> OpenProgram)
parseAtKeys = do
  keys <- sepBy1 (between (char '"') (char '"' *> spaces) parseKey)
                 (char ',' *> spaces)
  void $ char ']'
  Just ks <- pure $ NE.nonEmpty keys
  pure $ AtKeys ks

parseIterator :: Parser (Maybe OpenProgram -> OpenProgram)
parseIterator = Iterator <$ char ']'

parseAtIndices :: Parser (Maybe OpenProgram -> OpenProgram)
parseAtIndices = do
  Just idxs <- NE.nonEmpty <$> sepBy1 parseIdx (char ',' *> spaces)
  void $ char ']'
  pure $ AtIndices idxs

-- TODO this is a bit hacky
parseKey :: Parser BS8.ByteString
parseKey = do
  k <- BS8.pack <$> many1 (noneOf ".,|[](){} \"")
  guard (not $ BS8.null k) <?> "non-empty key"
  pure k

parseIdx :: Parser Int
parseIdx = do
  n <- maybe id (const negate)
       <$> optional (char '-')
  Just idx <- readMaybe <$> many1 digit
  pure . n $ fromInteger idx

--------------------------------------------------------------------------------
-- QuasiQuoter
--------------------------------------------------------------------------------

jq :: QuasiQuoter
jq = QuasiQuoter
  { quoteExp = parseExp
  , quotePat = const $ fail "Cannot be used here"
  , quoteType = const $ fail "Cannot be used here"
  , quoteDec = const $ fail "Cannot be used here"
  }

parseExp :: String -> Q Exp
parseExp inp = do
  Loc {loc_filename = file, loc_start = (line,col)} <- location

  let p = do
        pos <- getPosition
        setPosition $
          setSourceName (setSourceLine (setSourceColumn pos col) line) file
        parseProgram

  case runParser p () "" inp of
    Left err -> fail $ show err
    Right pgrm ->
      dataToExpQ (const Nothing `extQ` handleBS) pgrm

-- | The Data instance of ByteString doesn't play well with QQ for some reason,
-- so we have to manually convert it.
handleBS :: BS8.ByteString -> Maybe (Q Exp)
handleBS bs =
  Just . appE (varE 'BS8.pack) . litE . StringL $ BS8.unpack bs

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)
