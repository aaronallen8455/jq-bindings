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

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Data.Coerce
import           Data.Data
import           Data.Monoid (Any(..))
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

parseProgram :: Parser ClosedProgram
parseProgram =
  parseClosed <* skipSpace <* endOfInput

parseClosed :: Parser ClosedProgram
parseClosed = do
  skipSpace
  p <- choice [ parseParens
              , parseAsArray
              , parseDot
              ]
  choice
    [ parsePipe p
    , parseComma p
    , pure p
    ]

parsePipe :: ClosedProgram -> Parser ClosedProgram
parsePipe producer = do
  void $ skipSpace *> char8 '|'
  Pipe producer <$> parseClosed

parseComma :: ClosedProgram -> Parser ClosedProgram
parseComma p = do
  void $ skipSpace *> char8 ','
  Comma p <$> parseClosed

parseDot :: Parser ClosedProgram
parseDot = do
  Just '.' <- peekChar
  Dot <$> optional parseOpen

parseAsArray :: Parser ClosedProgram
parseAsArray = do
  char8 '[' *> skipSpace
  inside <- parseClosed
  void $ skipSpace *> char8 ']'
  Parens inside <$> optional parseOpen

parseParens :: Parser ClosedProgram
parseParens = do
  char8 '(' *> skipSpace
  inside <- parseClosed
  void $ skipSpace *> char8 ')'
  Parens inside <$> optional parseOpen

parseOpen :: Parser OpenProgram
parseOpen =
  choice
    [ parseAtKey
    , parseAtIndex
    , parseExplodeArray
    ]

parseAtKey :: Parser OpenProgram
parseAtKey = do
  key <- char8 '.' *> parseKey
  AtKey key <$> optional parseOpen

parseAtIndex :: Parser OpenProgram
parseAtIndex = do
  idx <- char8 '[' *> (parseIdx <* char8 ']')
  AtIdx idx <$> optional parseOpen

parseExplodeArray :: Parser OpenProgram
parseExplodeArray = do
  void $ string "[]"
  ExplodeArray <$> optional parseOpen

-- TODO this is probably not industrial grade
parseKey :: Parser BS8.ByteString
parseKey =
  takeTill . coerce
    $ foldMap (Any .) [isSpace, (`elem` '.':",|[](){}")]

parseIdx :: Parser Int
parseIdx = do
  (idx, frac) <- properFraction <$> scientific
  guard (frac == 0) <?> "Expected an integer for array index"
  pure $ fromInteger idx

--------------------------------------------------------------------------------
-- QuasiQuoter
--------------------------------------------------------------------------------

jq :: QuasiQuoter
jq = QuasiQuoter
  { quoteExp = \inp ->
      case parseOnly parseProgram (BS8.pack inp) of
        Left err -> fail err
        Right pgrm ->
          dataToExpQ (const Nothing `extQ` handleBS) pgrm
  , quotePat = const $ fail "Cannot be used here"
  , quoteType = const $ fail "Cannot be used here"
  , quoteDec = const $ fail "Cannot be used here"
  }

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
