{-# LANGUAGE MultiWayIf #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jq
  ( HasJv(..)
  , Jv
  , Kind(..)
  , KindSing(..)
  , KnownKind(..)
  , Path
  , PathComponent(..)
  , PrintOpts(..)
  , OpenProgram(..)
  , ClosedProgram(..)
  , SomeTypedJv(..)
  , TypedJv
  , array
  , arrayAppend
  , arrayConcat
  , arrayElems
  , arrayGet
  , arrayIndexes
  , arrayLength
  , arraySet
  , arraySlice
  , bool
  , boolValue
  , cast
  , contains
  , defPrintOpts
  , equal
  , execProgram
  , execProgramUnsafe
  , free
  , getKind
  , getPath
  , identical
  , isInteger
  , jq
  , kindEq
  , loadFile
  , loadFileCast
  , nullJv
  , number
  , numberValue
  , object
  , objectDelete
  , objectGet
  , objectHas
  , objectKeys
  , objectLength
  , objectMerge
  , objectMergeRecursive
  , objectSet
  , parse
  , render
  , setPath
  , string
  , stringIndexes
  , stringSlice
  , stringValue
  , withSomeTypedJv
  ) where

import qualified Control.Functor.Linear as L
import qualified Control.Monad as P
import qualified Control.Monad.IO.Class.Linear as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Foldable (for_)
import           Data.String
import           Data.Traversable (for)
import           Data.Type.Equality ((:~:)(..))
import           Foreign.C.Types
import qualified Prelude as P
import           Prelude.Linear
import qualified Unsafe.Linear as UL

import           Jq.Internal.Bindings
import           Jq.Program (ClosedProgram(..), OpenProgram(..), jq, renderClosedProgram)

--------------------------------------------------------------------------------
-- Typed Jvs
--------------------------------------------------------------------------------

data Kind
  = InvalidKind
  | NullKind
  | BoolKind
  | NumberKind
  | StringKind
  | ArrayKind
  | ObjectKind
  deriving (P.Show, P.Eq, P.Enum, P.Bounded)

data KindSing (k :: Kind) where
  NullS   :: KindSing 'NullKind
  BoolS   :: KindSing 'BoolKind
  NumberS :: KindSing 'NumberKind
  StringS :: KindSing 'StringKind
  ArrayS  :: KindSing 'ArrayKind
  ObjectS :: KindSing 'ObjectKind

kindSingLabel :: KindSing k -> BS.ByteString
kindSingLabel = \case
  NullS -> "null"
  BoolS -> "bool"
  NumberS -> "number"
  StringS -> "string"
  ArrayS -> "array"
  ObjectS -> "object"

kindEq :: KindSing k1 -> KindSing k2 -> Either BS.ByteString (k1 :~: k2)
kindEq NullS NullS = Right Refl
kindEq BoolS BoolS = Right Refl
kindEq NumberS NumberS = Right Refl
kindEq StringS StringS = Right Refl
kindEq ArrayS ArrayS = Right Refl
kindEq ObjectS ObjectS = Right Refl
kindEq expected actual =
  Left ("expected "
   P.<> kindSingLabel expected
   P.<> ", got "
   P.<> kindSingLabel actual
       )

class KnownKind (k :: Kind) where
  kindSing :: KindSing k
instance KnownKind 'NullKind where kindSing = NullS
instance KnownKind 'BoolKind where kindSing = BoolS
instance KnownKind 'NumberKind where kindSing = NumberS
instance KnownKind 'StringKind where kindSing = StringS
instance KnownKind 'ArrayKind where kindSing = ArrayS
instance KnownKind 'ObjectKind where kindSing = ObjectS

type role TypedJv representational
newtype TypedJv (k :: Kind) = TypedJv Jv

data SomeTypedJv where
  MkSomeTypedJv :: KnownKind k => TypedJv k %1 -> SomeTypedJv

withSomeTypedJv :: SomeTypedJv %1 -> (forall k. TypedJv k %1 -> KindSing k -> a) -> a
withSomeTypedJv (MkSomeTypedJv jv) f = f jv kindSing

-- | Frees the 'Jv' if it doesn't have the expected kind.
cast :: forall kind1 m jv. (KnownKind kind1, L.MonadIO m, HasJv jv)
     => jv %1
     -> m (Either (Ur BS.ByteString) (TypedJv kind1))
cast jv =
  typeJv jv L.>>= \case
    Left err -> L.pure (Left err)
    Right (MkSomeTypedJv @kind2 jv) ->
      case kindEq (kindSing @kind1) (kindSing @kind2) of
        Left mismatch -> L.do
          free jv
          L.pure (Left (Ur mismatch))
        Right Refl -> L.pure (Right jv)

--------------------------------------------------------------------------------
-- HasJv
--------------------------------------------------------------------------------

-- | Something that has a pointer to a `jv` value
class HasJv x where
  copy :: L.MonadIO m => x %1 -> m (x, x)
  forgetType :: x %1 -> Jv
  typeJv :: L.MonadIO m
         => x %1
         -> m (Either (Ur BS.ByteString) SomeTypedJv)

free :: (L.MonadIO m, HasJv x) => x %1 -> m ()
free = UL.toLinear $ \x -> L.liftSystemIO P.. jvFree P.$ forgetType x

instance HasJv Jv where
  copy = UL.toLinear $ \jv -> L.do
    jv2 <- L.liftSystemIO (jvCopy jv)
    L.pure (jv, jv2)
  forgetType = id
  typeJv jv = L.do
    (Ur kind, jv) <- getKind jv
    case kind of
      InvalidKind -> L.do
        invalidGetMessage jv L.>>= \case
          Nothing -> L.pure $ Left (Ur "Invalid JSON")
          Just msg -> L.do
            L.pure (Left msg)
      NullKind -> L.pure (Right (MkSomeTypedJv @'NullKind (TypedJv jv)))
      BoolKind -> L.pure (Right (MkSomeTypedJv @'BoolKind (TypedJv jv)))
      NumberKind -> L.pure (Right (MkSomeTypedJv @'NumberKind (TypedJv jv)))
      StringKind -> L.pure (Right (MkSomeTypedJv @'StringKind (TypedJv jv)))
      ArrayKind -> L.pure (Right (MkSomeTypedJv @'ArrayKind (TypedJv jv)))
      ObjectKind -> L.pure (Right (MkSomeTypedJv @'ObjectKind (TypedJv jv)))

instance KnownKind k => HasJv (TypedJv k) where
  copy (TypedJv jv) =
    (\(v, v2) -> (TypedJv v, TypedJv v2)) L.<$> copy jv
  forgetType (TypedJv jv) = jv
  typeJv jv = L.pure $ Right (MkSomeTypedJv jv)

instance HasJv SomeTypedJv where
  copy (MkSomeTypedJv tjv) = L.do
    (v1, v2) <- copy tjv
    L.pure (MkSomeTypedJv v1, MkSomeTypedJv v2)
  forgetType (MkSomeTypedJv (TypedJv jv)) = jv
  typeJv jv = L.pure $ Right jv

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: L.MonadIO m
      => BS.ByteString
      -> m Jv
parse str =
  L.liftSystemIO (BS.unsafeUseAsCString str jvParse)

invalidGetMessage :: L.MonadIO m => Jv %1 -> m (Maybe (Ur BS.ByteString))
invalidGetMessage = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  msg <- jvInvalidGetMsg jv
  msgK <- jvGetKind msg
  if msgK P.== jvKindString
     then do
       cStr <- jvStringValue msg
       bs <- BS.packCString cStr
       jvFree msg
       P.pure (Just (Ur bs))
     else do
       jvFree msg
       P.pure Nothing

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

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
render :: (L.MonadIO m, HasJv jv)
       => jv %1 -> PrintOpts -> m (Ur BS.ByteString)
render = UL.toLinear $ \jv opts -> L.liftSystemIOU P.$ P.do
  x <- jvDumpString (forgetType jv) (printOptsToFlags opts)
  cStr <- jvStringValue x
  bs <- BS.packCString cStr
  jvFree x
  P.pure bs

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

getKind :: L.MonadIO m => Jv %1 -> m (Ur Kind, Jv)
getKind = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  k <- jvGetKind jv
  P.pure P.$
    if | k P.== jvKindInvalid -> (Ur InvalidKind, jv)
       | k P.== jvKindNull -> (Ur NullKind, jv)
       | k P.== jvKindFalse -> (Ur BoolKind, jv)
       | k P.== jvKindTrue -> (Ur BoolKind, jv)
       | k P.== jvKindNumber -> (Ur NumberKind, jv)
       | k P.== jvKindString -> (Ur StringKind, jv)
       | k P.== jvKindArray -> (Ur ArrayKind, jv)
       | k P.== jvKindObject -> (Ur ObjectKind, jv)
       | otherwise -> (Ur InvalidKind, jv)

equal :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
equal = UL.toLinear $ \a -> UL.toLinear $ \b -> L.liftSystemIOU P.$
  toEnum P.. fromIntegral
    P.<$> jvEqual (forgetType a) (forgetType b)

identical :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
identical = UL.toLinear $ \a -> UL.toLinear $ \b -> L.liftSystemIOU P.$
  toEnum P.. fromIntegral
    P.<$> jvIdentical (forgetType a) (forgetType b)

contains :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
contains = UL.toLinear $ \a -> UL.toLinear $ \b -> L.liftSystemIOU P.$
  toEnum P.. fromIntegral
    P.<$> jvContains (forgetType a) (forgetType b)

loadFile :: L.MonadIO m => BS.ByteString -> m Jv
loadFile fileName = L.liftSystemIO P.$ do
  BS.unsafeUseAsCString fileName P.$ \cstr ->
    jvLoadFile cstr 0

loadFileCast :: (L.MonadIO m, KnownKind kind)
             => BS.ByteString -> m (Either (Ur BS.ByteString) (TypedJv kind))
loadFileCast fileName =
  loadFile fileName L.>>= typeJv L.>>= \case
    Left err -> L.pure (Left err)
    Right jv -> cast jv

nullJv :: L.MonadIO m => m (TypedJv 'NullKind)
nullJv = L.liftSystemIO (TypedJv P.<$> jvNull)

bool :: L.MonadIO m => Bool -> m (TypedJv 'BoolKind)
bool b = L.liftSystemIO P.$ do
  jv <- jvBool P.. fromIntegral P.$ fromEnum b
  P.pure (TypedJv jv)

boolValue :: L.MonadIO m => TypedJv 'BoolKind %1 -> m (Ur Bool)
boolValue jv = L.do
  true <- bool True
  equal jv true

number :: (L.MonadIO m, Real n) => n -> m (TypedJv 'NumberKind)
number n = L.liftSystemIO P.$ number' n

number' :: Real n => n -> IO (TypedJv 'NumberKind)
number' n = P.do
  x <- jvNumber (P.realToFrac n)
  P.pure (TypedJv x)

numberValue :: L.MonadIO m
            => TypedJv 'NumberKind %1
            -> m (TypedJv 'NumberKind, Ur Double)
numberValue = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  CDouble dbl <- jvNumberValue (forgetType jv)
  P.pure (jv, Ur dbl)

isInteger :: L.MonadIO m => TypedJv 'NumberKind %1 -> m (TypedJv 'NumberKind, Ur Bool)
isInteger = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  b <- toEnum P.. P.fromIntegral P.<$> jvIsInteger (forgetType jv)
  P.pure (jv, Ur b)

array :: (L.MonadIO m, HasJv jv) => [jv] %1 -> m (TypedJv 'ArrayKind)
array = UL.toLinear $ \es -> L.liftSystemIO (array' es)

array' :: HasJv jv => [jv] -> IO (TypedJv 'ArrayKind)
array' es = P.do
  arr <- jvArraySized (fromIntegral P.$ P.length es)
  for_ (es `P.zip` [0..]) P.$ \(e, idx) ->
    jvArraySet arr idx (forgetType e)
  P.pure (TypedJv arr)

arrayAppend :: (HasJv el, L.MonadIO m)
            => TypedJv 'ArrayKind %1 -> el %1 -> m (TypedJv 'ArrayKind)
arrayAppend = UL.toLinear $ \a@(TypedJv arr) ->
              UL.toLinear $ \el -> L.liftSystemIO P.$ do
  jvArrayAppend arr (forgetType el)
  P.pure a

arrayConcat
  :: L.MonadIO m
  => TypedJv 'ArrayKind %1
  -> TypedJv 'ArrayKind %1
  -> m (TypedJv 'ArrayKind)
arrayConcat = UL.toLinear $ \(TypedJv a) ->
              UL.toLinear $ \(TypedJv b) -> L.liftSystemIO P.$ do
  jvArrayConcat a b
  P.pure (TypedJv a)

arrayGet :: L.MonadIO m
         => TypedJv 'ArrayKind %1
         -> Int
         -> m (Either (Ur BS.ByteString) SomeTypedJv)
arrayGet arr ix = arrayGet' arr ix L.>>= typeJv

arrayGet' :: L.MonadIO m
          => TypedJv 'ArrayKind %1 -> Int -> m Jv
arrayGet' = UL.toLinear $ \arr idx -> L.liftSystemIO P.$ do
  jvArrayGet (forgetType arr) (fromIntegral idx)

arraySet :: (HasJv el, L.MonadIO m)
         => TypedJv 'ArrayKind %1 -> Int -> el %1 -> m (TypedJv 'ArrayKind)
arraySet = UL.toLinear $ \arr idx -> UL.toLinear $ \el -> L.liftSystemIO P.$ do
  jvArraySet (forgetType arr) (fromIntegral idx) (forgetType el)
  P.pure arr

arraySlice :: L.MonadIO m
           => TypedJv 'ArrayKind %1 -> Int -> Int -> m (TypedJv 'ArrayKind)
arraySlice = UL.toLinear $ \arr start end -> L.liftSystemIO P.$ do
  TypedJv P.<$>
    jvArraySlice (forgetType arr) (fromIntegral start) (fromIntegral end)

arrayElems :: L.MonadIO m
           => TypedJv 'ArrayKind %1 -> m [SomeTypedJv]
arrayElems arr = L.do
  (arr, arr2) <- copy arr
  Ur len <- arrayLength arr2

  let extract
        :: L.MonadIO m
        => (TypedJv 'ArrayKind, [SomeTypedJv]) %1
        -> Ur Int %1
        -> m (TypedJv 'ArrayKind, [SomeTypedJv])
      extract (arr, els) (Ur i) = L.do
        (arr, arr2) <- copy arr
        arrayGet arr2 i L.>>= \case
          Left (Ur _) -> L.pure (arr, els)
          Right el -> L.pure (arr, el : els)

  (arr, els) <-
    L.foldM extract (arr, []) (Ur P.<$> [len - 1, len - 2 .. 0])

  free arr
  L.pure els

arrayIndexes :: L.MonadIO m
             => TypedJv 'ArrayKind %1
             -> TypedJv 'ArrayKind %1
             -> m (TypedJv 'ArrayKind)
arrayIndexes = UL.toLinear $ \a -> UL.toLinear $ \b -> L.liftSystemIO P.$ do
  TypedJv P.<$>
    jvArrayIndexes (forgetType a) (forgetType b)

arrayLength :: L.MonadIO m
            => TypedJv 'ArrayKind %1
            -> m (Ur Int)
arrayLength = UL.toLinear $ \a -> L.liftSystemIO P.$
  Ur P.. fromIntegral P.<$> jvArrayLength (forgetType a)

string :: L.MonadIO m => BS.ByteString -> m (TypedJv 'StringKind)
string bs =
  L.liftSystemIO P.$ BS.unsafeUseAsCStringLen bs
    (\(cstr, len) -> P.fmap TypedJv P.$
        jvStringSized cstr (fromIntegral len)
    )

stringValue :: L.MonadIO m
            => TypedJv 'StringKind %1
            -> m (TypedJv 'StringKind, Ur BS.ByteString)
stringValue = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  bs <- BS.packCString P.=<< jvStringValue (forgetType jv)
  P.pure (jv, Ur bs)

stringIndexes :: L.MonadIO m
              => TypedJv 'StringKind %1
              -> TypedJv 'StringKind %1
              -> m (TypedJv 'ArrayKind)
stringIndexes = UL.toLinear $ \a -> UL.toLinear $ \b -> L.liftSystemIO P.$
  TypedJv P.<$> jvStringIndexes (forgetType a) (forgetType b)

stringSlice :: L.MonadIO m
            => TypedJv 'StringKind %1
            -> Int -> Int
            -> m (TypedJv 'StringKind)
stringSlice = UL.toLinear $ \jv start end -> L.liftSystemIO P.$
  TypedJv P.<$>
    jvStringSlice (forgetType jv) (fromIntegral start) (fromIntegral end)

object :: L.MonadIO m => m (TypedJv 'ObjectKind)
object = TypedJv L.<$> L.liftSystemIO jvObject

objectGet :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> m (Either (Ur BS.ByteString) SomeTypedJv)
objectGet jv key = objectGet' jv key L.>>= typeJv

-- TODO use Text for object keys?
objectGet' :: L.MonadIO m
           => TypedJv 'ObjectKind %1
           -> BS.ByteString
           -> m Jv
objectGet' = UL.toLinear $ \obj key -> L.liftSystemIO P.$ do
  jvKey <- BS.useAsCString key jvString
  jvObjectGet (forgetType obj) jvKey

objectHas :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> m (Ur Bool)
objectHas = UL.toLinear $ \obj key ->
            L.liftSystemIOU P.$ do
  keyJv <- BS.useAsCString key jvString
  P.toEnum P.. P.fromIntegral
    P.<$> jvObjectHas (forgetType obj) keyJv

objectSet :: (HasJv value, L.MonadIO m)
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> value %1
          -> m (TypedJv 'ObjectKind)
objectSet = UL.toLinear $ \obj key ->
            UL.toLinear $ \val -> L.liftSystemIO P.$ do
  jvKey <- BS.useAsCString key jvString
  jvObjectSet (forgetType obj) jvKey (forgetType val)
  P.pure obj

objectDelete :: L.MonadIO m
             => TypedJv 'ObjectKind %1
             -> BS.ByteString
             -> m (TypedJv 'ObjectKind)
objectDelete = UL.toLinear $ \obj key -> L.liftSystemIO P.$ do
  jvKey <- BS.useAsCString key jvString
  jvObjectDelete (forgetType obj) jvKey
  P.pure obj

objectLength :: L.MonadIO m
             => TypedJv 'ObjectKind %1
             -> m (Ur Int)
objectLength = UL.toLinear $ \obj -> L.liftSystemIOU P.$
  fromIntegral P.<$> jvObjectLength (forgetType obj)

objectMerge :: L.MonadIO m
            => TypedJv 'ObjectKind %1
            -> TypedJv 'ObjectKind %1
            -> m (TypedJv 'ObjectKind)
objectMerge = UL.toLinear $ \obj1 ->
              UL.toLinear $ \obj2 -> L.liftSystemIO P.$ do
  jvObjectMerge (forgetType obj1) (forgetType obj2)
  P.pure obj1

objectMergeRecursive
  :: L.MonadIO m
  => TypedJv 'ObjectKind %1
  -> TypedJv 'ObjectKind %1
  -> m (TypedJv 'ObjectKind)
objectMergeRecursive = UL.toLinear $ \obj1 ->
                       UL.toLinear $ \obj2 -> L.liftSystemIO P.$ do
  jvObjectMergeRecursive (forgetType obj1) (forgetType obj2)
  P.pure obj1

objectKeys
  :: L.MonadIO m
  => TypedJv 'ObjectKind %1
  -> m [TypedJv 'StringKind]
objectKeys = UL.toLinear $ \obj -> L.liftSystemIO P.$ do
  keys <- jvKeys (forgetType obj)
  keys' <- jvCopy keys
  arrLen <- jvArrayLength keys
  result <- for [0 .. arrLen P.- 1] P.$ \i -> do
    keys'' <- jvCopy keys'
    TypedJv P.<$> jvArrayGet keys'' i
  jvFree keys'
  P.pure result

data PathComponent
  = ArrayIdx Int
  | ObjectKey BS.ByteString

instance P.Num PathComponent where
  ArrayIdx a + ArrayIdx b = ArrayIdx (a + b)
  a + _ = a
  ArrayIdx a * ArrayIdx b = ArrayIdx (a * b)
  a * _ = a
  abs (ArrayIdx a) = ArrayIdx (abs a)
  abs a = a
  signum a = a
  fromInteger = ArrayIdx P.. P.fromIntegral
  negate (ArrayIdx i) = ArrayIdx (P.negate i)
  negate a = a

instance IsString PathComponent where
  fromString = ObjectKey P.. fromString

type Path = [PathComponent]

pathToJv :: Path -> P.IO (TypedJv 'ArrayKind)
pathToJv path =
  P.traverse componentToJv path
    P.>>= array'

componentToJv :: PathComponent -> P.IO Jv
componentToJv (ArrayIdx i) = P.do
  TypedJv n <- number' i
  P.pure n
componentToJv (ObjectKey key) =
  BS.useAsCString key jvString

getPath :: (HasJv jv, L.MonadIO m) => jv %1 -> Path -> m (Either (Ur BS.ByteString) SomeTypedJv)
getPath jv path = getPath' jv path L.>>= typeJv

getPath' :: (HasJv jv, L.MonadIO m) => jv %1 -> Path -> m Jv
getPath' = UL.toLinear $ \jv path -> L.liftSystemIO P.$ do
  TypedJv pathJv <- pathToJv path
  jvGetpath (forgetType jv) pathJv
  P.pure (forgetType jv)

setPath :: (HasJv root, HasJv val, L.MonadIO m)
        => root %1 -> Path -> val %1 -> m (Either (Ur BS.ByteString) SomeTypedJv)
setPath root path jv = setPath' root path jv L.>>= typeJv

setPath' :: (HasJv root, HasJv val, L.MonadIO m)
         => root %1 -> Path -> val %1 -> m Jv
setPath' = UL.toLinear $ \jv path -> UL.toLinear $ \val -> L.liftSystemIO P.$ do
  TypedJv pathJv <- pathToJv path
  jvSetpath (forgetType jv) pathJv (forgetType val)
  P.pure (forgetType jv)

--------------------------------------------------------------------------------
-- Program Execution
--------------------------------------------------------------------------------

execProgram
  :: (HasJv val, L.MonadIO m)
  => ClosedProgram -> val %1 -> m [SomeTypedJv]
execProgram = execProgramUnsafe P.. renderClosedProgram

compile :: L.MonadIO m => JqState %1 -> BS.ByteString -> m (Maybe JqState)
compile = UL.toLinear $ \jq pgrm -> L.liftSystemIO P.$ do
  didCompile <- BS.useAsCString pgrm (jqCompile jq)
  case didCompile of
    0 -> do
      jqTeardown jq
      P.pure Nothing
    _ -> P.pure P.$ Just jq

start :: (L.MonadIO m, HasJv jv) => JqState %1 -> jv %1 -> m JqState
start = UL.toLinear $ \jq -> UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  jqStart jq (forgetType jv) 0
  P.pure jq

teardown :: L.MonadIO m => JqState %1 -> m ()
teardown = UL.toLinear $ L.liftSystemIO P.. jqTeardown

next :: L.MonadIO m => JqState %1 -> m (JqState, Jv)
next = UL.toLinear $ \jq -> L.liftSystemIO P.$ do
  jv <- jqNext jq
  P.pure (jq, jv)

execProgramUnsafe
  :: (HasJv val, L.MonadIO m)
  => BS.ByteString -> val %1 -> m [SomeTypedJv]
execProgramUnsafe pgrm jv = L.do
  jq <- L.liftSystemIO jqInit
  compile jq pgrm L.>>= \case
    Nothing -> L.do
      free jv
      L.pure []
    Just jq -> L.do
      jq <- start jq jv
      let loop jq = L.do
            (jq, jv) <- next jq
            typeJv jv L.>>= \case
              Left (Ur _) -> L.pure ([], jq)
              Right v ->
                (\(els, jq) -> (v : els, jq))
                L.<$> loop jq
      (res, jq) <- loop jq
      teardown jq
      L.pure res
