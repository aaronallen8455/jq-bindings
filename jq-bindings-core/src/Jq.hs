{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
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
  , renderMultiple
  , setPath
  , string
  , stringIndexes
  , stringSlice
  , stringValue
  , withSomeTypedJv
  ) where

import qualified Control.Functor.Linear as CL
import qualified Control.Monad.IO.Class.Linear as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Functor.Linear as DL
import           Data.String
import           Data.Type.Equality ((:~:)(..))
import           Foreign.C.Types
import qualified Prelude.Linear as L
import           Prelude.Linear (Ur(..))
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
  deriving (Show, Eq, Enum, Bounded)

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
   <> kindSingLabel expected
   <> ", got "
   <> kindSingLabel actual
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
  typeJv jv CL.>>= \case
    Left err -> CL.pure (Left err)
    Right (MkSomeTypedJv @kind2 jv) ->
      case kindEq (kindSing @kind1) (kindSing @kind2) of
        Left mismatch -> CL.do
          free jv
          CL.pure (Left (Ur mismatch))
        Right Refl -> CL.pure (Right jv)

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
free = UL.toLinear L.$ \x -> L.liftSystemIO . jvFree $ forgetType x

instance HasJv Jv where
  copy = UL.toLinear L.$ \jv -> CL.do
    jv2 <- L.liftSystemIO (jvCopy jv)
    CL.pure (jv, jv2)

  forgetType = L.id

  typeJv jv = CL.do
    (Ur kind, jv) <- getKind jv
    case kind of
      InvalidKind -> CL.do
        invalidGetMessage jv CL.>>= \case
          Nothing -> CL.pure L.$ Left (Ur "invalid JSON")
          Just msg -> CL.do
            CL.pure (Left msg)
      NullKind -> CL.pure (Right (MkSomeTypedJv @'NullKind (TypedJv jv)))
      BoolKind -> CL.pure (Right (MkSomeTypedJv @'BoolKind (TypedJv jv)))
      NumberKind -> CL.pure (Right (MkSomeTypedJv @'NumberKind (TypedJv jv)))
      StringKind -> CL.pure (Right (MkSomeTypedJv @'StringKind (TypedJv jv)))
      ArrayKind -> CL.pure (Right (MkSomeTypedJv @'ArrayKind (TypedJv jv)))
      ObjectKind -> CL.pure (Right (MkSomeTypedJv @'ObjectKind (TypedJv jv)))

instance KnownKind k => HasJv (TypedJv k) where
  copy (TypedJv jv) =
    (\(v, v2) -> (TypedJv v, TypedJv v2)) CL.<$> copy jv
  forgetType (TypedJv jv) = jv
  typeJv jv = CL.pure L.$ Right (MkSomeTypedJv jv)

instance HasJv SomeTypedJv where
  copy (MkSomeTypedJv tjv) = CL.do
    (v1, v2) <- copy tjv
    CL.pure (MkSomeTypedJv v1, MkSomeTypedJv v2)
  forgetType (MkSomeTypedJv (TypedJv jv)) = jv
  typeJv jv = CL.pure L.$ Right jv

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: L.MonadIO m
      => BS.ByteString
      -> m Jv
parse str =
  L.liftSystemIO
    (BS.unsafeUseAsCStringLen str
      (\(cstr, len) -> jvParseSized cstr $ fromIntegral len))

-- | If the Jv is invalid, get its error message
invalidGetMessage :: L.MonadIO m => Jv %1 -> m (Maybe (Ur BS.ByteString))
invalidGetMessage jv = CL.do
  msg <- invalidGetMsg' jv
  cast msg CL.>>= \case
    Left (Ur _) -> CL.pure Nothing
    Right msgStr -> CL.do
      (msgStr, Ur bs) <- stringValue msgStr
      free msgStr
      CL.pure (Just (Ur bs))

invalidGetMsg' :: L.MonadIO m => Jv %1 -> m Jv
invalidGetMsg' = UL.toLinear L.$ L.liftSystemIO . jvInvalidGetMsg

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
  foldr (Bits..|.) 0 $ concat
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

render :: (L.MonadIO m, HasJv jv)
       => jv %1 -> PrintOpts -> m (Ur BS.ByteString)
render jv opts = CL.do
  x <- dumpString jv opts
  (jv, Ur bs) <- stringValue x
  free jv
  CL.pure (Ur bs)

dumpString :: (L.MonadIO m, HasJv jv)
           => jv %1 -> PrintOpts -> m (TypedJv 'StringKind)
dumpString = UL.toLinear L.$ \jv opts -> L.liftSystemIO $
  TypedJv <$>
    jvDumpString (forgetType jv) (printOptsToFlags opts)

renderMultiple :: (L.MonadIO m, HasJv jv)
               => [jv] %1 -> PrintOpts -> m (Ur [BS.ByteString])
renderMultiple jvs opts = CL.do
  (Ur urs) <- L.move DL.<$> DL.traverse (L.flip render opts) jvs
  CL.pure (Ur $ fmap (\(Ur x) -> x) urs)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

getKind :: L.MonadIO m => Jv %1 -> m (Ur Kind, Jv)
getKind = UL.toLinear L.$ \jv -> L.liftSystemIO $ do
  k <- jvGetKind jv
  pure $
    if | k == jvKindInvalid -> (Ur InvalidKind, jv)
       | k == jvKindNull -> (Ur NullKind, jv)
       | k == jvKindFalse -> (Ur BoolKind, jv)
       | k == jvKindTrue -> (Ur BoolKind, jv)
       | k == jvKindNumber -> (Ur NumberKind, jv)
       | k == jvKindString -> (Ur StringKind, jv)
       | k == jvKindArray -> (Ur ArrayKind, jv)
       | k == jvKindObject -> (Ur ObjectKind, jv)
       | otherwise -> (Ur InvalidKind, jv)

equal :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
equal = UL.toLinear L.$ \a -> UL.toLinear L.$ \b -> L.liftSystemIOU $
  toEnum . fromIntegral
    <$> jvEqual (forgetType a) (forgetType b)

identical :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
identical = UL.toLinear L.$ \a -> UL.toLinear L.$ \b -> L.liftSystemIOU $
  toEnum . fromIntegral
    <$> jvIdentical (forgetType a) (forgetType b)

contains :: (HasJv a, HasJv b, L.MonadIO m) => a %1 -> b %1 -> m (Ur Bool)
contains = UL.toLinear L.$ \a -> UL.toLinear L.$ \b -> L.liftSystemIOU $
  toEnum . fromIntegral
    <$> jvContains (forgetType a) (forgetType b)

-- | The given file path can be absolute or relative.
loadFile :: L.MonadIO m => BS.ByteString -> m (Either (Ur BS.ByteString) SomeTypedJv)
loadFile fileName = CL.do
  jv <- L.liftSystemIO $ do
    BS.useAsCString fileName $ \cstr ->
      jvLoadFile cstr 0
  typeJv jv CL.>>= \case
    Left (Ur err) -> CL.pure (Left (Ur err))
    Right (MkSomeTypedJv @kind jv) ->
      case kindSing @kind of
        -- Check if the result is a singleton array and unwrap if so
        ArrayS -> CL.do
          (jv, jv2) <- copy jv
          Ur len <- arrayLength jv2
          if len == 1
             then arrayGet jv 0
             else CL.pure (Right (MkSomeTypedJv jv))
        _ -> CL.pure (Right (MkSomeTypedJv jv))

-- | Parses the file and returns an error if the result is not of the expected kind.
loadFileCast :: (L.MonadIO m, KnownKind kind)
             => BS.ByteString -> m (Either (Ur BS.ByteString) (TypedJv kind))
loadFileCast fileName =
  loadFile fileName CL.>>= \case
    Left err -> CL.pure (Left err)
    Right jv -> cast jv

nullJv :: L.MonadIO m => m (TypedJv 'NullKind)
nullJv = L.liftSystemIO (TypedJv <$> jvNull)

bool :: L.MonadIO m => Bool -> m (TypedJv 'BoolKind)
bool b = L.liftSystemIO $ do
  jv <- jvBool . fromIntegral $ fromEnum b
  pure (TypedJv jv)

boolValue :: L.MonadIO m => TypedJv 'BoolKind %1 -> m (Ur Bool)
boolValue jv = CL.do
  true <- bool True
  equal jv true

number :: (L.MonadIO m, Real n) => n -> m (TypedJv 'NumberKind)
number n = L.liftSystemIO $ number' n

number' :: Real n => n -> IO (TypedJv 'NumberKind)
number' n = do
  x <- jvNumber (realToFrac n)
  pure (TypedJv x)

numberValue :: L.MonadIO m
            => TypedJv 'NumberKind %1
            -> m (TypedJv 'NumberKind, Ur Double)
numberValue = UL.toLinear L.$ \jv -> L.liftSystemIO $ do
  CDouble dbl <- jvNumberValue (forgetType jv)
  pure (jv, Ur dbl)

isInteger :: L.MonadIO m => TypedJv 'NumberKind %1 -> m (TypedJv 'NumberKind, Ur Bool)
isInteger = UL.toLinear L.$ \jv -> L.liftSystemIO $ do
  b <- toEnum . fromIntegral <$> jvIsInteger (forgetType jv)
  pure (jv, Ur b)

array :: forall m jv. (L.MonadIO m, HasJv jv)
      => [jv] %1 -> m (TypedJv 'ArrayKind)
array es = CL.do
  (Ur len, es) <- CL.pure (L.length es)
  arr <- arraySized len

  let insert :: (TypedJv 'ArrayKind, Ur Int) %1 -> jv %1 -> m (TypedJv 'ArrayKind, Ur Int)
      insert (arr, Ur ix) e = CL.do
        arr <- arraySet arr ix e
        CL.pure (arr, Ur $! ix + 1)

  (arr, Ur _) <- CL.foldM insert (arr, Ur 0) es
  CL.pure arr

arraySized :: L.MonadIO m => Int -> m (TypedJv 'ArrayKind)
arraySized size = L.liftSystemIO $
  TypedJv <$> jvArraySized (fromIntegral size)

arrayAppend :: (HasJv el, L.MonadIO m)
            => TypedJv 'ArrayKind %1 -> el %1 -> m (TypedJv 'ArrayKind)
arrayAppend = UL.toLinear L.$ \a@(TypedJv arr) ->
              UL.toLinear L.$ \el -> L.liftSystemIO $ do
  jvArrayAppend arr (forgetType el)
  pure a

arrayConcat
  :: L.MonadIO m
  => TypedJv 'ArrayKind %1
  -> TypedJv 'ArrayKind %1
  -> m (TypedJv 'ArrayKind)
arrayConcat = UL.toLinear L.$ \(TypedJv a) ->
              UL.toLinear L.$ \(TypedJv b) -> L.liftSystemIO $ do
  jvArrayConcat a b
  pure (TypedJv a)

arrayGet :: L.MonadIO m
         => TypedJv 'ArrayKind %1
         -> Int
         -> m (Either (Ur BS.ByteString) SomeTypedJv)
arrayGet arr ix = arrayGet' arr ix CL.>>= typeJv

arrayGet' :: L.MonadIO m
          => TypedJv 'ArrayKind %1 -> Int -> m Jv
arrayGet' = UL.toLinear L.$ \arr idx -> L.liftSystemIO $ do
  jvArrayGet (forgetType arr) (fromIntegral idx)

arraySet :: (HasJv el, L.MonadIO m)
         => TypedJv 'ArrayKind %1 -> Int -> el %1 -> m (TypedJv 'ArrayKind)
arraySet = UL.toLinear L.$ \arr idx -> UL.toLinear L.$ \el -> L.liftSystemIO $ do
  jvArraySet (forgetType arr) (fromIntegral idx) (forgetType el)
  pure arr

arraySlice :: L.MonadIO m
           => TypedJv 'ArrayKind %1 -> Int -> Int -> m (TypedJv 'ArrayKind)
arraySlice = UL.toLinear L.$ \arr start end -> L.liftSystemIO $ do
  TypedJv <$>
    jvArraySlice (forgetType arr) (fromIntegral start) (fromIntegral end)

arrayElems :: L.MonadIO m
           => TypedJv 'ArrayKind %1 -> m [SomeTypedJv]
arrayElems arr = CL.do
  (arr, arr2) <- copy arr
  Ur len <- arrayLength arr2

  let extract
        :: L.MonadIO m
        => (TypedJv 'ArrayKind, [SomeTypedJv]) %1
        -> Ur Int %1
        -> m (TypedJv 'ArrayKind, [SomeTypedJv])
      extract (arr, els) (Ur i) = CL.do
        (arr, arr2) <- copy arr
        arrayGet arr2 i CL.>>= \case
          Left (Ur _) -> CL.pure (arr, els)
          Right el -> CL.pure (arr, el : els)

  (arr, els) <-
    CL.foldM extract (arr, []) (Ur <$> [len - 1, len - 2 .. 0])

  free arr
  CL.pure els

arrayIndexes :: L.MonadIO m
             => TypedJv 'ArrayKind %1
             -> TypedJv 'ArrayKind %1
             -> m (TypedJv 'ArrayKind)
arrayIndexes = UL.toLinear L.$ \a -> UL.toLinear L.$ \b -> L.liftSystemIO $ do
  TypedJv <$>
    jvArrayIndexes (forgetType a) (forgetType b)

arrayLength :: L.MonadIO m
            => TypedJv 'ArrayKind %1
            -> m (Ur Int)
arrayLength = UL.toLinear L.$ \a -> L.liftSystemIO $
  Ur . fromIntegral <$> jvArrayLength (forgetType a)

-- | Construct a JSON string. Input should not contain null bytes.
string :: L.MonadIO m => BS.ByteString -> m (TypedJv 'StringKind)
string bs =
  L.liftSystemIO $ BS.unsafeUseAsCStringLen bs
    (\(cstr, len) -> fmap TypedJv $
        jvStringSized cstr (fromIntegral len)
    )

stringValue :: L.MonadIO m
            => TypedJv 'StringKind %1
            -> m (TypedJv 'StringKind, Ur BS.ByteString)
stringValue = UL.toLinear L.$ \jv -> L.liftSystemIO $ do
  bs <- BS.packCString =<< jvStringValue (forgetType jv)
  pure (jv, Ur bs)

stringIndexes :: L.MonadIO m
              => TypedJv 'StringKind %1
              -> TypedJv 'StringKind %1
              -> m (TypedJv 'ArrayKind)
stringIndexes = UL.toLinear L.$ \a -> UL.toLinear L.$ \b -> L.liftSystemIO $
  TypedJv <$> jvStringIndexes (forgetType a) (forgetType b)

stringSlice :: L.MonadIO m
            => TypedJv 'StringKind %1
            -> Int -> Int
            -> m (TypedJv 'StringKind)
stringSlice = UL.toLinear L.$ \jv start end -> L.liftSystemIO $
  TypedJv <$>
    jvStringSlice (forgetType jv) (fromIntegral start) (fromIntegral end)

object :: L.MonadIO m => m (TypedJv 'ObjectKind)
object = TypedJv CL.<$> L.liftSystemIO jvObject

objectGet :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> m (Either (Ur BS.ByteString) SomeTypedJv)
objectGet jv key = objectGet' jv key CL.>>= typeJv

-- TODO use Text for object keys?
objectGet' :: L.MonadIO m
           => TypedJv 'ObjectKind %1
           -> BS.ByteString
           -> m Jv
objectGet' = UL.toLinear L.$ \obj key -> L.liftSystemIO $ do
  jvKey <- BS.useAsCString key jvString
  jvObjectGet (forgetType obj) jvKey

objectHas :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> m (Ur Bool)
objectHas = UL.toLinear L.$ \obj key ->
            L.liftSystemIOU $ do
  keyJv <- BS.useAsCString key jvString
  toEnum . fromIntegral
    <$> jvObjectHas (forgetType obj) keyJv

objectSet :: (HasJv value, L.MonadIO m)
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> value %1
          -> m (TypedJv 'ObjectKind)
objectSet = UL.toLinear L.$ \obj key ->
            UL.toLinear L.$ \val -> L.liftSystemIO $ do
  jvKey <- BS.useAsCString key jvString
  jvObjectSet (forgetType obj) jvKey (forgetType val)
  pure obj

objectDelete :: L.MonadIO m
             => TypedJv 'ObjectKind %1
             -> BS.ByteString
             -> m (TypedJv 'ObjectKind)
objectDelete = UL.toLinear L.$ \obj key -> L.liftSystemIO $ do
  jvKey <- BS.useAsCString key jvString
  jvObjectDelete (forgetType obj) jvKey
  pure obj

objectLength :: L.MonadIO m
             => TypedJv 'ObjectKind %1
             -> m (Ur Int)
objectLength = UL.toLinear L.$ \obj -> L.liftSystemIOU $
  fromIntegral <$> jvObjectLength (forgetType obj)

objectMerge :: L.MonadIO m
            => TypedJv 'ObjectKind %1
            -> TypedJv 'ObjectKind %1
            -> m (TypedJv 'ObjectKind)
objectMerge = UL.toLinear L.$ \obj1 ->
              UL.toLinear L.$ \obj2 -> L.liftSystemIO $ do
  jvObjectMerge (forgetType obj1) (forgetType obj2)
  pure obj1

objectMergeRecursive
  :: L.MonadIO m
  => TypedJv 'ObjectKind %1
  -> TypedJv 'ObjectKind %1
  -> m (TypedJv 'ObjectKind)
objectMergeRecursive = UL.toLinear L.$ \obj1 ->
                       UL.toLinear L.$ \obj2 -> L.liftSystemIO $ do
  jvObjectMergeRecursive (forgetType obj1) (forgetType obj2)
  pure obj1

objectKeys
  :: L.MonadIO m
  => TypedJv 'ObjectKind %1
  -> m [TypedJv 'StringKind]
objectKeys obj = CL.do
  keys <- objectKeys' obj
  els <- arrayElems keys
  CL.pure L.$ L.map (\(MkSomeTypedJv (TypedJv jv)) -> TypedJv jv) els

objectKeys'
  :: L.MonadIO m
  => TypedJv 'ObjectKind %1
  -> m (TypedJv 'ArrayKind)
objectKeys' = UL.toLinear L.$ \(TypedJv obj) -> L.liftSystemIO $
  TypedJv <$> jvKeys obj

data PathComponent where
  ArrayIdx :: Int -> PathComponent
  ObjectKey :: BS.ByteString -> PathComponent

-- | This is merely for ergonomics
instance Num PathComponent where
  ArrayIdx a + ArrayIdx b = ArrayIdx (a + b)
  a + _ = a
  ArrayIdx a * ArrayIdx b = ArrayIdx (a * b)
  a * _ = a
  abs (ArrayIdx a) = ArrayIdx (abs a)
  abs a = a
  signum a = a
  fromInteger = ArrayIdx . fromIntegral
  negate (ArrayIdx i) = ArrayIdx (negate i)
  negate a = a

instance IsString PathComponent where
  fromString = ObjectKey . fromString

type Path = [PathComponent]

pathToJv :: L.MonadIO m => Path -> m (TypedJv 'ArrayKind)
pathToJv path =
  DL.traverse componentToJv path
    CL.>>= array

componentToJv :: L.MonadIO m => PathComponent %1 -> m Jv
componentToJv (ArrayIdx i) = CL.do
  TypedJv n <- number i
  CL.pure n
componentToJv (ObjectKey key) = CL.do
  TypedJv str <- string key
  CL.pure str

getPath :: (HasJv jv, L.MonadIO m) => jv %1 -> Path -> m (Either (Ur BS.ByteString) SomeTypedJv)
getPath jv path = CL.do
  pathJv <- pathToJv path
  getPath' jv pathJv CL.>>= typeJv

getPath' :: (HasJv jv, L.MonadIO m) => jv %1 -> TypedJv 'ArrayKind %1 -> m Jv
getPath' = UL.toLinear L.$ \jv -> UL.toLinear L.$ \(TypedJv pathJv) -> L.liftSystemIO $ do
  jvGetpath (forgetType jv) pathJv
  pure (forgetType jv)

setPath :: (HasJv root, HasJv val, L.MonadIO m)
        => root %1 -> Path -> val %1 -> m (Either (Ur BS.ByteString) SomeTypedJv)
setPath root path jv = CL.do
  pathJv <- pathToJv path
  setPath' root pathJv jv CL.>>= typeJv

setPath' :: (HasJv root, HasJv val, L.MonadIO m)
         => root %1 -> TypedJv 'ArrayKind %1 -> val %1 -> m Jv
setPath' = UL.toLinear L.$ \jv -> UL.toLinear L.$ \(TypedJv pathJv) -> UL.toLinear L.$ \val -> L.liftSystemIO $ do
  jvSetpath (forgetType jv) pathJv (forgetType val)
  pure (forgetType jv)

--------------------------------------------------------------------------------
-- Program Execution
--------------------------------------------------------------------------------

execProgram
  :: (HasJv val, L.MonadIO m)
  => ClosedProgram -> val %1 -> m [SomeTypedJv]
execProgram = execProgramUnsafe . renderClosedProgram

compile :: L.MonadIO m => JqState %1 -> BS.ByteString -> m (Maybe JqState)
compile = UL.toLinear L.$ \jq pgrm -> L.liftSystemIO $ do
  didCompile <- BS.useAsCString pgrm (jqCompile jq)
  case didCompile of
    0 -> do
      jqTeardown jq
      pure Nothing
    _ -> pure $ Just jq

start :: (L.MonadIO m, HasJv jv) => JqState %1 -> jv %1 -> m JqState
start = UL.toLinear L.$ \jq -> UL.toLinear L.$ \jv -> L.liftSystemIO $ do
  jqStart jq (forgetType jv) 0
  pure jq

teardown :: L.MonadIO m => JqState %1 -> m ()
teardown = UL.toLinear L.$ L.liftSystemIO . jqTeardown

next :: L.MonadIO m => JqState %1 -> m (JqState, Jv)
next = UL.toLinear L.$ \jq -> L.liftSystemIO $ do
  jv <- jqNext jq
  pure (jq, jv)

execProgramUnsafe
  :: (HasJv val, L.MonadIO m)
  => BS.ByteString -> val %1 -> m [SomeTypedJv]
execProgramUnsafe pgrm jv = CL.do
  jq <- L.liftSystemIO jqInit
  compile jq pgrm CL.>>= \case
    Nothing -> CL.do
      free jv
      CL.pure []
    Just jq -> CL.do
      jq <- start jq jv
      let loop jq = CL.do
            (jq, jv) <- next jq
            typeJv jv CL.>>= \case
              Left (Ur _) -> CL.pure ([], jq)
              Right v ->
                (\(els, jq) -> (v : els, jq))
                CL.<$> loop jq
      (res, jq) <- loop jq
      teardown jq
      CL.pure res
