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

import           Control.Applicative (optional)
import           Control.Monad
import           Text.Parsec hiding (optional)
import           Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS8
import           Data.Data
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- | AST for expressions that can be further extended with additional key
-- queries, etc.
data OpenProgram
  = AtKey BS8.ByteString (Maybe OpenProgram)
  | AtIdx Int (Maybe OpenProgram)
  | ExplodeArray (Maybe OpenProgram)
  deriving (Show, Eq, Data)

-- | AST for "top level" programs that cannot be extended with field accessors
-- or array indices.
data ClosedProgram
  = Dot (Maybe OpenProgram)
  | Pipe ClosedProgram ClosedProgram
  | Comma ClosedProgram ClosedProgram
  | AsArray ClosedProgram (Maybe OpenProgram)
  | Parens ClosedProgram (Maybe OpenProgram)
  deriving (Show, Eq, Data)

-- This is not a complete embedding of the jq language. The AST does not
-- include literal expressions or many other fancy things.

renderOpenProgram :: OpenProgram -> BS8.ByteString
renderOpenProgram = \case
  AtKey k p -> "." <> k <> foldMap renderOpenProgram p
  AtIdx i p -> "[" <> BS8.pack (show i) <> "]" <> foldMap renderOpenProgram p
  ExplodeArray p -> "[]" <> foldMap renderOpenProgram p

-- | Produce a 'ByteString' that can be fed to jq as a valid program
renderClosedProgram :: ClosedProgram -> BS8.ByteString
renderClosedProgram = \case
  Dot (Just p@AtKey{}) -> renderOpenProgram p
  Dot p -> "." <> foldMap renderOpenProgram p
  Pipe a b -> renderClosedProgram a <> " | " <> renderClosedProgram b
  Comma a b -> renderClosedProgram a <> ", " <> renderClosedProgram b
  AsArray p o -> "[" <> renderClosedProgram p <> "]" <> foldMap renderOpenProgram o
  Parens p o -> "(" <> renderClosedProgram p <> ")" <> foldMap renderOpenProgram o

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
           , parseDot
           ]
    )
    `chainl1` (Comma <$ char ',')
    `chainl1` (Pipe <$ char '|')

parseDot :: Parser ClosedProgram
parseDot = do
  -- Use lookahead because AtKey also needs to parse '.'
  void $ lookAhead (char '.')
  fmap Dot
      $ try (fmap Just parseOpen)
    <|> (Nothing <$ char '.')

parseAsArray :: Parser ClosedProgram
parseAsArray = do
  inside <-
    between (char '[' *> spaces)
            (spaces <* char ']')
            parseClosed
  AsArray inside <$> optional parseOpen

parseParens :: Parser ClosedProgram
parseParens = do
  inside <-
    between (char '(' *> spaces)
            (spaces <* char ')')
            parseClosed
  Parens inside <$> optional parseOpen

parseOpen :: Parser OpenProgram
parseOpen = do
  c <- choice
        [ parseAtKey
        , parseExplodeArray
        , parseAtIndex
        ]
  c <$> optional parseOpen

parseAtKey :: Parser (Maybe OpenProgram -> OpenProgram)
parseAtKey = do
  key <- char '.' *> parseKey
  pure $ AtKey key

parseExplodeArray :: Parser (Maybe OpenProgram -> OpenProgram)
parseExplodeArray = do
  void . try $ string "[]" -- use try b/c AtIndex also matches [
  pure ExplodeArray

parseAtIndex :: Parser (Maybe OpenProgram -> OpenProgram)
parseAtIndex = do
  idx <- between (char '[') (char ']') parseIdx
  pure $ AtIdx idx

-- TODO this is a bit hacky
parseKey :: Parser BS8.ByteString
parseKey = do
  k <- BS8.pack <$> many1 (noneOf ".,|[](){} ")
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
