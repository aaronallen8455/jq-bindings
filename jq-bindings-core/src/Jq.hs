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
  , arrayGet
  , arrayIndexes
  , arrayLength
  , arraySet
  , arraySlice
  , bool
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
  , parseMaybe
  , parseTyped
  , render
  , setPath
  , string
  , stringIndexes
  , stringSlice
  , stringValue
  , typeJv
  , withSomeTypedJv
  ) where

import qualified Control.Functor.Linear as L
import qualified Control.Monad.IO.Class.Linear as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
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
  | FalseKind
  | TrueKind
  | NumberKind
  | StringKind
  | ArrayKind
  | ObjectKind
  deriving (P.Show, P.Eq, P.Enum, P.Bounded)

data KindSing (k :: Kind) where
  NullS   :: KindSing 'NullKind
  FalseS  :: KindSing 'FalseKind
  TrueS   :: KindSing 'TrueKind
  NumberS :: KindSing 'NumberKind
  StringS :: KindSing 'StringKind
  ArrayS  :: KindSing 'ArrayKind
  ObjectS :: KindSing 'ObjectKind

kindEq :: KindSing k1 -> KindSing k2 -> Maybe (k1 :~: k2)
kindEq NullS NullS = Just Refl
kindEq FalseS FalseS = Just Refl
kindEq TrueS TrueS = Just Refl
kindEq NumberS NumberS = Just Refl
kindEq StringS StringS = Just Refl
kindEq ArrayS ArrayS = Just Refl
kindEq ObjectS ObjectS = Just Refl
kindEq _ _ = Nothing

class KnownKind (k :: Kind) where
  kindSing :: KindSing k
instance KnownKind 'NullKind where kindSing = NullS
instance KnownKind 'FalseKind where kindSing = FalseS
instance KnownKind 'TrueKind where kindSing = TrueS
instance KnownKind 'NumberKind where kindSing = NumberS
instance KnownKind 'StringKind where kindSing = StringS
instance KnownKind 'ArrayKind where kindSing = ArrayS
instance KnownKind 'ObjectKind where kindSing = ObjectS

type role TypedJv representational
newtype TypedJv (k :: Kind) = TypedJv Jv

data SomeTypedJv where
  MkSomeTypedJv :: KindSing k -> TypedJv k %1 -> SomeTypedJv

-- | Returns nothing if invalid
typeJv :: L.MonadIO m => Jv %1 -> m (Maybe SomeTypedJv)
typeJv = UL.toLinear $ \jv -> L.liftSystemIO P.$ typeJv' jv

typeJv' :: Jv -> P.IO (Maybe SomeTypedJv)
typeJv' jv = do
  mSomeJv <- typeJv'' jv
  case mSomeJv of
    Nothing -> jvFree jv -- free if invalid
    Just _ -> P.pure ()
  P.pure mSomeJv

-- Doesn't free if invalid so you can get the message out
typeJv'' :: Jv -> P.IO (Maybe SomeTypedJv)
typeJv'' jv = do
  k <- toEnum P.. P.fromIntegral P.<$> jvGetKind jv
  case k of
    InvalidKind -> P.pure Nothing
    NullKind    -> P.pure P.. Just P.$ MkSomeTypedJv NullS (TypedJv jv)
    FalseKind   -> P.pure P.. Just P.$ MkSomeTypedJv FalseS (TypedJv jv)
    TrueKind    -> P.pure P.. Just P.$ MkSomeTypedJv TrueS (TypedJv jv)
    NumberKind  -> P.pure P.. Just P.$ MkSomeTypedJv NumberS (TypedJv jv)
    StringKind  -> P.pure P.. Just P.$ MkSomeTypedJv StringS (TypedJv jv)
    ArrayKind   -> P.pure P.. Just P.$ MkSomeTypedJv ArrayS (TypedJv jv)
    ObjectKind  -> P.pure P.. Just P.$ MkSomeTypedJv ObjectS (TypedJv jv)

withSomeTypedJv :: SomeTypedJv %1 -> (forall k. TypedJv k %1 -> KindSing k -> a) -> a
withSomeTypedJv (MkSomeTypedJv sing jv) f = f jv sing

-- | Frees the 'Jv' if it doesn't have the expected kind.
cast :: forall kind m. (KnownKind kind, L.MonadIO m)
     => SomeTypedJv %1 -> m (Maybe (TypedJv kind))
cast (MkSomeTypedJv sing1 jv) =
  let sing2 = kindSing @kind
   in case kindEq sing1 sing2 of
        Nothing -> free jv L.>> L.pure Nothing
        Just Refl -> L.pure (Just jv)

--------------------------------------------------------------------------------
-- HasJv
--------------------------------------------------------------------------------

-- | Something that has a pointer to a `jv` value
class HasJv x where
  copy :: L.MonadIO m => x %1 -> m (x, x)
  forgetType :: x %1 -> Jv

free :: (L.MonadIO m, HasJv x) => x %1 -> m ()
free = UL.toLinear $ \x -> L.liftSystemIO P.. jvFree P.$ forgetType x

instance HasJv Jv where
  copy = UL.toLinear $ \jv -> L.do
    jv2 <- L.liftSystemIO (jvCopy jv)
    L.pure (jv, jv2)
  forgetType = id

instance HasJv (TypedJv k) where
  copy (TypedJv jv) =
    (\(v, v2) -> (TypedJv v, TypedJv v2)) L.<$> copy jv
  forgetType (TypedJv jv) = jv

instance HasJv SomeTypedJv where
  copy (MkSomeTypedJv k tjv) = L.do
    (v1, v2) <- copy tjv
    L.pure (MkSomeTypedJv k v1, MkSomeTypedJv k v2)
  forgetType (MkSomeTypedJv _ (TypedJv jv)) = jv

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: L.MonadIO m
      => BS.ByteString
      -> m (Either (Ur BS.ByteString) SomeTypedJv)
parse str = L.liftSystemIO P.$
  validate P.=<< BS.useAsCString str jvParse

parseMaybe :: L.MonadIO m => BS.ByteString -> m (Maybe SomeTypedJv)
parseMaybe str =
  either (\(Ur _) -> Nothing) Just L.<$> parse str

parseTyped :: (KnownKind kind, L.MonadIO m)
           => BS.ByteString -> m (Maybe (TypedJv kind))
parseTyped str =
  parseMaybe str L.>>= \case
    Nothing -> L.pure Nothing
    Just tjv -> cast tjv

validate :: Jv -> IO (Either (Ur BS.ByteString) SomeTypedJv)
validate jv = do
  mTjv <- typeJv'' jv
  -- check if it's valid
  case mTjv of
    Just tjv -> P.pure (Right tjv)
    Nothing -> do
      msg <- jvInvalidGetMsg jv
      msgK <- jvGetKind msg
      -- check if there's an error message
      if msgK P.== jvKindNull
         then do
           jvFree msg
           P.pure (Left $ Ur "Invalid JSON")
         else do
           cStr <- jvStringValue msg
           bs <- BS.packCString cStr
           jvFree msg
           P.pure (Left $ Ur bs)

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
  k <- toEnum P.. fromIntegral P.<$> jvGetKind jv
  P.pure (Ur k, jv)

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

nullJv :: L.MonadIO m => m (TypedJv 'NullKind)
nullJv = L.liftSystemIO (TypedJv P.<$> jvNull)

bool :: L.MonadIO m => Bool -> m SomeTypedJv
bool b = L.liftSystemIO P.$ do
  jv <- jvBool P.. fromIntegral P.$ fromEnum b
  P.pure P.$ if b
     then MkSomeTypedJv TrueS (TypedJv jv)
     else MkSomeTypedJv FalseS (TypedJv jv)

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
         => TypedJv 'ArrayKind %1 -> Int -> m (Maybe SomeTypedJv)
arrayGet = UL.toLinear $ \arr idx -> L.liftSystemIO P.$ do
  el <- jvArrayGet (forgetType arr) (fromIntegral idx)
  typeJv' el

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

-- TODO arrayElems :: TypedJv 'ArrayKind %1 -> m [SomeTypedJv]

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
  L.liftSystemIO P.$ BS.useAsCString bs (P.fmap TypedJv P.. jvString)

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

-- TODO use Text for object keys?
objectGet :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> BS.ByteString
          -> m (Maybe SomeTypedJv)
objectGet = UL.toLinear $ \obj key -> L.liftSystemIO P.$ do
  jvKey <- BS.useAsCString key jvString
  typeJv' P.=<< jvObjectGet (forgetType obj) jvKey

objectHas :: L.MonadIO m
          => TypedJv 'ObjectKind %1
          -> TypedJv 'StringKind %1
          -> m (Ur Bool)
objectHas = UL.toLinear $ \obj ->
            UL.toLinear $ \key -> L.liftSystemIOU P.$ do
  P.toEnum P.. P.fromIntegral
    P.<$> jvObjectHas (forgetType obj) (forgetType key)

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

getPath :: (HasJv jv, L.MonadIO m) => jv %1 -> Path -> m (Maybe SomeTypedJv)
getPath = UL.toLinear $ \jv path -> L.liftSystemIO P.$ do
  TypedJv pathJv <- pathToJv path
  jvGetpath (forgetType jv) pathJv
  typeJv' (forgetType jv)

setPath :: (HasJv root, HasJv val, L.MonadIO m)
        => root %1 -> Path -> val %1 -> m (Maybe SomeTypedJv)
setPath = UL.toLinear $ \jv path -> UL.toLinear $ \val -> L.liftSystemIO P.$ do
  TypedJv pathJv <- pathToJv path
  jvSetpath (forgetType jv) pathJv (forgetType val)
  typeJv' (forgetType jv)

--------------------------------------------------------------------------------
-- Program Execution
--------------------------------------------------------------------------------

execProgram
  :: (HasJv val, L.MonadIO m)
  => ClosedProgram -> val %1 -> m [SomeTypedJv]
execProgram = execProgramUnsafe P.. renderClosedProgram

execProgramUnsafe
  :: (HasJv val, L.MonadIO m)
  => BS.ByteString -> val %1 -> m [SomeTypedJv]
execProgramUnsafe pgrm = UL.toLinear $ \jv -> L.liftSystemIO P.$ do
  jq <- jqInit
  didCompile <- BS.useAsCString pgrm P.$ jqCompile jq
  case didCompile of
    0 -> do
      jqTeardown jq
      jvFree (forgetType jv)
      P.pure []

    _ -> do
      jqStart jq (forgetType jv) 0 -- what are these flags for?
      let loop = do
            mV <- typeJv' P.=<< jqNext jq
            case mV of
              Nothing -> P.pure []
              Just v -> (v :) P.<$> loop
      res <- loop
      jqTeardown jq
      P.pure res
