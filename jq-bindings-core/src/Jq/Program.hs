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

-- | AST for "top level" programs that cannot be extended with field accessors
-- or array indices.
data ClosedProgram
  = Identity (Maybe OpenProgram)
  | Pipe ClosedProgram ClosedProgram
  | Comma ClosedProgram ClosedProgram
  | AsArray ClosedProgram (Maybe OpenProgram)
  | Parens ClosedProgram (Maybe OpenProgram)
  | RecursiveDescent (Maybe OpenProgram)
  | Object [(BS8.ByteString, ClosedProgram)]
  | Select ClosedProgram (Maybe OpenProgram)
  -- ^ filter by a predicate although the expression can really be anything
  | Predicate ClosedProgram ComparisonOp ClosedProgram
  | BooleanCombo BoolOp ClosedProgram ClosedProgram
  -- ^ treat as the the conjuntion or disjunction of two booleans
  | LiteralExpr Literal
  deriving (Show, Eq, Data)

data BoolOp
  = And
  | Or
  deriving (Show, Eq, Data)

data ComparisonOp
  = Equal
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  deriving (Show, Eq, Data)

data Literal
  = LitString BS8.ByteString
  | LitInt Int
  | LitDouble Double
  | LitBool Bool
  | LitNull
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

  AsArray p o ->
    "[" <> renderClosedProgram p <> "]" <> foldMap renderOpenProgram o

  Parens p o ->
    "(" <> renderClosedProgram p <> ")" <> foldMap renderOpenProgram o

  RecursiveDescent p -> ".." <> foldMap renderOpenProgram p

  Object obj -> "{" <> BS8.intercalate "," (renderPairs <$> obj) <> "}"
    where renderPairs (key, p) = "\"" <> key <> "\":" <> renderClosedProgram p

  Select pred rest ->
    "select(" <> renderClosedProgram pred <> ")"
    <> foldMap renderOpenProgram rest

  Predicate l op r ->
    renderClosedProgram l <> " " <> renderComparisonOp op
    <> " " <> renderClosedProgram r

  BooleanCombo op l r ->
    renderClosedProgram l <> " " <> renderBoolOp op
    <> " " <> renderClosedProgram r

  LiteralExpr l ->
    case l of
      LitString bs -> "\"" <> bs <> "\""
      LitInt int -> BS8.pack $ show int
      LitDouble dbl -> BS8.pack $ show dbl
      LitBool b -> if b then "true" else "false"
      LitNull -> "null"

renderComparisonOp :: ComparisonOp -> BS8.ByteString
renderComparisonOp = \case
  Equal -> "=="
  LessThan -> "<"
  LessThanEqual -> "<="
  GreaterThan -> ">"
  GreaterThanEqual -> ">="

renderBoolOp :: BoolOp -> BS8.ByteString
renderBoolOp = \case
  And -> "and"
  Or -> "or"

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser = Parsec String ()

parseProgram :: Parser ClosedProgram
parseProgram =
  parseClosed <* spaces <* eof

parseClosed :: Parser ClosedProgram
parseClosed = do
  between spaces spaces parseClosedInner
    `chainl1` (BooleanCombo <$> parseBoolOp)
    `chainl1` (Comma <$ char ',')
    `chainl1` (Pipe <$ char '|')

parseClosedInner :: Parser ClosedProgram
parseClosedInner = do
  let parseExpr =
        choice [ parseParens
               , parseAsArray
               , parseRecursiveDescent
               , parseIdentity
               , parseObject
               , parseSelect
               , LiteralExpr <$> parseLiteral
               ]

  expr <- parseExpr
  spaces
  -- Check for a comparison operator
  mComparisonOp <- optional parseComparisonOp
  case mComparisonOp of
    Nothing -> pure expr
    Just compOp -> do
      spaces
      expr2 <- parseExpr
      pure $ Predicate expr compOp expr2

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

parseObject :: Parser ClosedProgram
parseObject = do
  let parseKeys = do
        mQStart <- optional (char '"')
        key <- parseKey
        mQEnd <- optional (char '"')
        guard (mQStart == mQEnd) <?> "object key quotes"
        spaces
        -- if no colon, use the key as the path
        mColon <- optional $ char ':'
        case mColon of
          Nothing -> pure (key, Identity (Just $ AtKeys (pure key) Nothing))
          _ -> do
            spaces
            pgrm <- (parseClosedInner <* spaces)
                      `chainl1` (Pipe <$ (char '|' <* spaces))
            pure (key, pgrm)

  pairs <-
    between (char '{' *> spaces)
            (char '}' *> spaces)
            (sepBy1 parseKeys (char ',' *> spaces))

  pure $ Object pairs

parseSelect :: Parser ClosedProgram
parseSelect = do
  void $ string "select"
  spaces
  pred <- between (char '(') (char ')') parseClosed
  Select pred <$> optional (parseOpen False True)

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
  unless isFirst $
    void (char '.')
  AtKeys . pure <$> parseKey

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
  Just idxs <- NE.nonEmpty <$> sepBy1 parseInt (char ',' *> spaces)
  void $ char ']'
  pure $ AtIndices idxs

-- TODO this is a bit hacky
parseKey :: Parser BS8.ByteString
parseKey = do
  k <- BS8.pack <$> many1 (noneOf ".,|[](){} \":")
  guard (not $ BS8.null k) <?> "non-empty key"
  pure k

parseInt :: Parser Int
parseInt = do
  n <- maybe id (const negate)
       <$> optional (char '-')
  Just int <- readMaybe <$> many1 digit
  pure $ n int

parseDouble :: Parser Double
parseDouble = do
  n <- maybe id (const negate)
       <$> optional (char '-')
  whole <- many1 digit
  void $ char '.'
  Just dbl <- readMaybe . ((whole <> ".") <>) <$> many1 digit
  pure $ n dbl

parseBoolOp :: Parser BoolOp
parseBoolOp =
  choice [ And <$ string "and"
         , Or <$ string "or"
         ]

parseComparisonOp :: Parser ComparisonOp
parseComparisonOp =
  choice [ Equal <$ string "=="
         , LessThanEqual <$ try (string "<=")
         , LessThan <$ string "<"
         , GreaterThanEqual <$ try (string ">=")
         , GreaterThan <$ string ">"
         ]

parseLiteral :: Parser Literal
parseLiteral =
  choice [ LitString <$> parseLitString
         , LitDouble <$> try parseDouble
         , LitInt <$> parseInt
         , LitBool <$> parseBool
         , LitNull <$ string "null"
         ]
  where
    parseLitString = BS8.pack <$>
      between (char '"') (char '"') (many1 $ satisfy (/= '"'))
      -- TODO doesn't handle escaped quotes
    parseBool =
      choice [ True <$ string "true"
             , False <$ string "false"
             ]

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
