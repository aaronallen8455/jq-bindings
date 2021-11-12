{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Jq.Encode
  ( Encoder(..)
  , real
  , int
  , double
  , string
  , byteString
  , text
  , bool
  , nullable
  , Field(..)
  , object
  , objField
  , objFieldOmitNull
  , array
  , coerceEnc
  ) where

import qualified Control.Functor.Linear as FL
import qualified Control.Monad.IO.Class.Linear as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Coerce
import           Data.Foldable (toList)
import           Data.Functor.Contravariant
import qualified Data.Functor.Linear as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Prelude.Linear as L

import qualified Jq

newtype Encoder a =
  MkEncoder
    { runEncoder :: forall m. L.MonadIO m => a -> m Jq.SomeTypedJv }

instance Contravariant Encoder where
  contramap f encoder = MkEncoder $ runEncoder encoder . f

real :: Real n => Encoder n
real = MkEncoder $ \i -> FL.do
  jv <- Jq.number i
  FL.pure (Jq.MkSomeTypedJv jv)

int :: Encoder Int
int = real

double :: Encoder Double
double = real

string :: Encoder String
string = MkEncoder $ \str -> FL.do
  jv <- Jq.string (BS8.pack str)
  FL.pure (Jq.MkSomeTypedJv jv)

byteString :: Encoder BS.ByteString
byteString = MkEncoder $ \bs -> FL.do
  jv <- Jq.string bs
  FL.pure (Jq.MkSomeTypedJv jv)

text :: Encoder T.Text
text = MkEncoder $ \t -> FL.do
  jv <- Jq.string (TE.encodeUtf8 t)
  FL.pure (Jq.MkSomeTypedJv jv)

bool :: Encoder Bool
bool = MkEncoder (\b -> FL.fmap Jq.MkSomeTypedJv (Jq.bool b))

nullable :: Encoder a -> Encoder (Maybe a)
nullable encoder = MkEncoder $ \mb ->
  case mb of
    Nothing -> FL.do
      jv <- Jq.nullJv
      FL.pure (Jq.MkSomeTypedJv jv)
    Just a -> runEncoder encoder a

newtype Field obj =
  MkField
    { runField :: forall m. L.MonadIO m
               => obj
               -> Jq.TypedJv 'Jq.ObjectKind %1
               -> m (Jq.TypedJv 'Jq.ObjectKind)
    }

object :: [Field obj] -> Encoder obj
object fields = MkEncoder $ \obj -> FL.do
  jv <- Jq.object
  jv <-
    FL.foldM
      (\jv (L.Ur field) -> runField field obj jv)
      jv
      (L.Ur <$> fields)
  FL.pure (Jq.MkSomeTypedJv jv)

objField :: String -> (obj -> a) -> Encoder a -> Field obj
objField key accessor encoder = MkField $ \obj jv -> FL.do
  let val = accessor obj
  valJv <- runEncoder encoder val
  Jq.objectSet jv (BS8.pack key) valJv

objFieldOmitNull :: String -> (obj -> Maybe a) -> Encoder a -> Field obj
objFieldOmitNull key accessor encoder = MkField $ \obj jv -> FL.do
  case accessor obj of
    Nothing -> FL.pure jv
    Just val ->
      runField (objField key (const val) encoder) obj jv

array :: Foldable f => Encoder a -> Encoder (f a)
array encoder = MkEncoder $ \fa -> FL.do
  jvs <- L.traverse (\(L.Ur a) -> runEncoder encoder a) (L.Ur <$> toList fa)
  arr <- Jq.array jvs
  FL.pure (Jq.MkSomeTypedJv arr)

coerceEnc :: Coercible a b => Encoder a -> Encoder b
coerceEnc = contramap coerce
