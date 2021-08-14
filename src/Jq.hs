{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Jq
  ( parse
  , PrintOpts(..)
  , defPrintOpts
  , render
  , HasJv(..)
  , Kind(..)
  , KindSing(..)
  , getKind
  , typeJv
  , withSomeTypedJv
  , equal
  , identical
  , contains
  , nullJv
  , bool
  , string
  , array
  , arrayAppend
  , arrayConcat
  , arrayGet
  , arraySet
  ) where

import qualified Control.Functor.Linear as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.Unrestricted.Linear (Ur(..))
import           Foreign.C.String
import qualified Prelude as P
import           Prelude.Linear
import qualified System.IO.Linear as L
import qualified Unsafe.Linear as UL

import           Jq.Internal.Bindings

parse :: BS.ByteString
      -> L.IO (Either (Ur BS.ByteString) Jv)
parse str = L.fromSystemIO $
  validate P.=<< BS.useAsCString str jvParse

parseMaybe :: BS.ByteString -> L.IO (Maybe Jv)
parseMaybe str =
  either (\(Ur _) -> Nothing) Just L.<$> parse str

validate :: Jv -> IO (Either (Ur BS.ByteString) Jv)
validate jv = do
  k <- jvGetKind jv
  -- check if it's valid
  if k P./= jvKindInvalid
     then P.pure (Right jv)
     else do -- invalid
       msg <- jvInvalidGetMsg jv
       jvFree jv
       msgK <- jvGetKind msg
       -- check if there's an error message
       if msgK P.== jvKindNull
          then do
            jvFree msg
            P.pure (Left $ Ur "Invalid JSON")
          else do
            cStr <- jvStringValue msg
            jvFree msg
            bs <- BS.packCString cStr
            P.pure (Left $ Ur bs)

class HasJv x where
  copy :: x %1 -> L.IO (x, x)
  free :: x %1 -> L.IO ()
  forgetType :: x %1 -> Jv

instance HasJv Jv where
  copy = UL.toLinear $ \jv -> L.do
    jv2 <- L.fromSystemIO (jvCopy jv)
    L.pure (jv, jv2)
  free = L.fromSystemIO . UL.toLinear jvFree
  forgetType = id

instance HasJv (TypedJv k) where
  copy (TypedJv jv) =
    (\(v, v2) -> (TypedJv v, TypedJv v2)) L.<$> copy jv
  free (TypedJv jv) = free jv
  forgetType (TypedJv jv) = jv

instance HasJv SomeTypedJv where
  copy (MkSomeTypedJv k tjv) = L.do
    (v1, v2) <- copy tjv
    L.pure (MkSomeTypedJv k v1, MkSomeTypedJv k v2)
  free (MkSomeTypedJv _ tjv) = free tjv
  forgetType (MkSomeTypedJv _ (TypedJv jv)) = jv

data PrintOpts =
  MkPrintOpts
    { printPretty   :: Bool
    , printAscII    :: Bool
    , printColor    :: Bool
    , printSorted   :: Bool
    , printInvalid  :: Bool
    , printRefCount :: Bool
    , printTab      :: Bool
    , printIsatty   :: Bool
    , printSpace0   :: Bool
    , printSpace1   :: Bool
    , printSpace2   :: Bool
    } deriving Show

defPrintOpts :: PrintOpts
defPrintOpts =
  MkPrintOpts
    { printPretty   = False, printAscII    = False, printColor    = False
    , printSorted   = False, printInvalid  = False, printRefCount = False
    , printTab      = False, printIsatty   = False, printSpace0   = False
    , printSpace1   = False, printSpace2   = False
    }

printOptsToFlags :: PrintOpts -> JvPrintFlags
printOptsToFlags MkPrintOpts{..} =
  P.foldr (Bits..|.) 0 P.$ concat
    [ [ jvPrintPretty | printPretty ]
    , [ jvPrintAscII | printAscII ]
    , [ jvPrintColor | printColor ]
    , [ jvPrintSorted | printSorted ]
    , [ jvPrintInvalid | printInvalid ]
    , [ jvPrintRefCount | printRefCount ]
    , [ jvPrintTab | printTab ]
    , [ jvPrintIsatty | printIsatty ]
    , [ jvPrintSpace0 | printSpace0 ]
    , [ jvPrintSpace1 | printSpace1 ]
    , [ jvPrintSpace2 | printSpace2 ]
    ]

-- Consumes the Jv argument
render :: HasJv jv => jv %1 -> PrintOpts -> L.IO (Ur BS.ByteString)
render = UL.toLinear $ \jv opts -> L.fromSystemIO $ P.do
  x <- jvDumpString (forgetType jv) (printOptsToFlags opts)
  cStr <- jvStringValue x
  jvFree x
  bs <- BS.packCString cStr
  P.pure (Ur bs)

data Kind
  = InvalidKind
  | NullKind
  | FalseKind
  | TrueKind
  | NumberKind
  | StringKind
  | ArrayKind
  | ObjectKind
  deriving (P.Show, P.Eq, P.Enum, P.Bounded)

data KindSing (k :: Kind) where
  InvalidS :: KindSing 'InvalidKind
  NullS   :: KindSing 'NullKind
  FalseS  :: KindSing 'FalseKind
  TrueS   :: KindSing 'TrueKind
  NumberS :: KindSing 'NumberKind
  StringS :: KindSing 'StringKind
  ArrayS  :: KindSing 'ArrayKind
  ObjectS :: KindSing 'ObjectKind

type role TypedJv representational
newtype TypedJv (k :: Kind) = TypedJv Jv

data SomeTypedJv where
  MkSomeTypedJv :: KindSing k -> TypedJv k %1 -> SomeTypedJv

-- Needs to be linear?
withSomeTypedJv :: SomeTypedJv -> (forall k. KindSing k -> TypedJv k -> a) -> a
withSomeTypedJv (MkSomeTypedJv sing jv) f = f sing jv

getKind :: Jv %1 -> L.IO (Ur Kind, Jv)
getKind = UL.toLinear $ \jv -> L.fromSystemIO $ do
  k <- toEnum P.. fromIntegral P.<$> jvGetKind jv
  P.pure (Ur k, jv)

typeJv :: Jv %1 -> L.IO SomeTypedJv
typeJv = UL.toLinear $ \jv -> L.fromSystemIO $ typeJv' jv

typeJv' :: Jv -> P.IO SomeTypedJv
typeJv' jv = do
  k <- toEnum P.. P.fromIntegral P.<$> jvGetKind jv
  P.pure P.$ case k of
    InvalidKind -> MkSomeTypedJv InvalidS (TypedJv jv)
    NullKind    -> MkSomeTypedJv NullS (TypedJv jv)
    FalseKind   -> MkSomeTypedJv FalseS (TypedJv jv)
    TrueKind    -> MkSomeTypedJv TrueS (TypedJv jv)
    NumberKind  -> MkSomeTypedJv NumberS (TypedJv jv)
    StringKind  -> MkSomeTypedJv StringS (TypedJv jv)
    ArrayKind   -> MkSomeTypedJv ArrayS (TypedJv jv)
    ObjectKind  -> MkSomeTypedJv ObjectS (TypedJv jv)

equal :: (HasJv a, HasJv b) => a %1 -> b %1 -> L.IO (Ur Bool)
equal = UL.toLinear $ \a -> UL.toLinear $ \b -> L.fromSystemIO $
  Ur P.. toEnum P.. fromIntegral
    P.<$> jvEqual (forgetType a) (forgetType b)

identical :: (HasJv a, HasJv b) => a %1 -> b %1 -> L.IO (Ur Bool)
identical = UL.toLinear $ \a -> UL.toLinear $ \b -> L.fromSystemIO $
  Ur P.. toEnum P.. fromIntegral
    P.<$> jvIdentical (forgetType a) (forgetType b)

contains :: (HasJv a, HasJv b) => a %1 -> b %1 -> L.IO (Ur Bool)
contains = UL.toLinear $ \a -> UL.toLinear $ \b -> L.fromSystemIO $
  Ur P.. toEnum P.. fromIntegral
    P.<$> jvContains (forgetType a) (forgetType b)

nullJv :: L.IO (TypedJv 'NullKind)
nullJv = L.fromSystemIO (TypedJv P.<$> jvNull)

bool :: Bool -> L.IO Jv
bool b = L.fromSystemIO $ jvBool P.. fromIntegral P.$ fromEnum b

string :: BS.ByteString -> L.IO (TypedJv 'StringKind)
string bs =
  L.fromSystemIO $ BS.useAsCString bs (P.fmap TypedJv P.. jvString)

array :: [Jv] %1 -> L.IO (TypedJv 'ArrayKind)
array = UL.toLinear $ \es -> L.fromSystemIO $ do
  arr <- jvArraySized (fromIntegral P.$ P.length es)
  for_ (es `P.zip` [0..]) P.$ \(e, idx) ->
    jvArraySet arr idx e
  P.pure (TypedJv arr)

arrayAppend :: HasJv el
            => TypedJv 'ArrayKind %1 -> el %1 -> L.IO (TypedJv 'ArrayKind)
arrayAppend = UL.toLinear $ \a@(TypedJv arr) ->
              UL.toLinear $ \el -> L.fromSystemIO $ do
  jvArrayAppend arr (forgetType el)
  P.pure a

arrayConcat
  :: TypedJv 'ArrayKind %1
  -> TypedJv 'ArrayKind %1
  -> L.IO (TypedJv 'ArrayKind)
arrayConcat = UL.toLinear $ \(TypedJv a) ->
              UL.toLinear $ \(TypedJv b) -> L.fromSystemIO $ do
  jvArrayConcat a b
  P.pure (TypedJv a)

arrayGet :: TypedJv 'ArrayKind %1 -> Int -> L.IO (Maybe SomeTypedJv)
arrayGet = UL.toLinear $ \arr idx -> L.fromSystemIO $ do
  el <- jvArrayGet (forgetType arr) (fromIntegral idx)
  tjv <- typeJv' el
  P.pure P.$ withSomeTypedJv tjv P.$ \case
    InvalidS -> \_ -> Nothing
    _ -> \_ -> Just tjv

arraySet :: HasJv el
         => TypedJv 'ArrayKind %1 -> Int -> el %1 -> L.IO (TypedJv 'ArrayKind)
arraySet = UL.toLinear $ \arr idx -> UL.toLinear $ \el -> L.fromSystemIO $ do
  jvArraySet (forgetType arr) (fromIntegral idx) (forgetType el)
  P.pure arr
