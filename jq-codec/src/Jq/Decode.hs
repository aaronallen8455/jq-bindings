{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
module Jq.Decode
  ( Decoder(..)
  , int
  , double
  , string
  , byteString
  , text
  , bool
  , nullable
  , requiredKey
  , nullableKey
  , optionalKey
  , optionalNullableKey
  , array
  ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Functor.Linear as FL
import           Control.Monad (ap, join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Control.Monad.IO.Class.Linear as L
import qualified Prelude.Linear as L

import qualified Jq

newtype Decoder a =
  MkDecoder
    { runDecoder
        :: forall m jv. (L.MonadIO m, Jq.HasJv jv)
        => jv %1
        -> m (L.Ur (Either BS.ByteString a))
    }

instance Functor Decoder where
  fmap f decoder = MkDecoder $ \jv ->
    FL.fmap (\(L.Ur x) -> L.Ur (fmap f x)) (runDecoder decoder jv)

instance Applicative Decoder where
  pure x = MkDecoder $ \jv -> FL.do
    Jq.free jv
    FL.pure (L.Ur (Right x))
  (<*>) = ap

instance Monad Decoder where
  ma >>= amb = MkDecoder $ \jv -> FL.do
    (jv, jv2) <- Jq.copy jv
    runDecoder ma jv2 FL.>>= \case
      L.Ur (Left err) -> FL.do
        Jq.free jv
        FL.pure (L.Ur (Left err))
      L.Ur (Right a) -> FL.do
        runDecoder (amb a) jv

instance Alternative Decoder where
  empty = MkDecoder $ \jv -> FL.do
    Jq.free jv
    FL.pure (L.Ur $ Left "empty")
  ma <|> mb = MkDecoder $ \jv -> FL.do
    (jv, jv2) <- Jq.copy jv
    runDecoder ma jv2 FL.>>= \case
      L.Ur (Left _) -> FL.do
        runDecoder mb jv
      L.Ur (Right a) -> FL.do
        Jq.free jv
        FL.pure (L.Ur $ Right a)

--------------------------------------------------------------------------------
-- Primitive Decoders
--------------------------------------------------------------------------------

int :: Decoder Int
int = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, L.Ur isInt) <- Jq.isInteger jv
      if isInt
         then FL.do
           (jv, L.Ur v) <- Jq.numberValue jv
           Jq.free jv
           FL.pure (L.Ur (Right $ floor v))
         else FL.do
           Jq.free jv
           FL.pure (L.Ur (Left "Number is not an integral"))

double :: Decoder Double
double = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, L.Ur v) <- Jq.numberValue jv
      Jq.free jv
      FL.pure (L.Ur (Right v))

string :: Decoder String
string = BS8.unpack <$> byteString

byteString :: Decoder BS.ByteString
byteString = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, L.Ur bs) <- Jq.stringValue jv
      Jq.free jv
      FL.pure (L.Ur (Right bs))

text :: Decoder T.Text
text = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, L.Ur bs) <- Jq.stringValue jv
      Jq.free jv
      case TE.decodeUtf8' bs of
        Left _ -> FL.pure (L.Ur (Left "not a utf-8 encoded string"))
        Right txt -> FL.pure (L.Ur (Right txt))

bool :: Decoder Bool
bool = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      L.Ur b <- Jq.boolValue jv
      FL.pure (L.Ur $ Right b)

nullable :: Decoder a -> Decoder (Maybe a)
nullable decoder = MkDecoder $ \jv ->
  Jq.typeJv jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right (Jq.MkSomeTypedJv @k jv) ->
      case Jq.kindEq (Jq.kindSing @k) Jq.NullS of
        Right _ -> FL.do
          Jq.free jv
          FL.pure (L.Ur $ Right Nothing)
        Left _ -> runDecoder (Just <$> decoder) jv

-- | Required object field
requiredKey :: BS.ByteString -> Decoder a -> Decoder a
requiredKey key decoder = MkDecoder $ \jv -> FL.do
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, jv2) <- Jq.copy jv
      L.Ur hasKey <- Jq.objectHas jv2 key
      if hasKey
         then FL.do
           Jq.objectGet jv key FL.>>= \case
             Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
             Right jv -> runDecoder decoder jv
         else FL.do
           Jq.free jv
           FL.pure (L.Ur . Left $ "missing required key: " <> key)

-- | Nullable field
nullableKey :: BS.ByteString -> Decoder a -> Decoder (Maybe a)
nullableKey key decoder = requiredKey key (nullable decoder)

optionalKey :: BS.ByteString -> Decoder a -> Decoder (Maybe a)
optionalKey key decoder = MkDecoder $ \jv -> FL.do
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, jv2) <- Jq.copy jv
      L.Ur hasKey <- Jq.objectHas jv2 key
      if hasKey
         then FL.do
           Jq.objectGet jv key FL.>>= \case
             Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
             Right jv -> runDecoder (Just <$> decoder) jv
         else FL.do
           Jq.free jv
           FL.pure (L.Ur $ Right Nothing)

optionalNullableKey :: BS.ByteString -> Decoder a -> Decoder (Maybe a)
optionalNullableKey key decoder =
  join <$> optionalKey key (nullable decoder)

-- TODO use Vector instead?
array :: Decoder a -> Decoder [a]
array decoder = MkDecoder $ \jv ->
  Jq.cast jv FL.>>= \case
    Left (L.Ur err) -> FL.pure (L.Ur $ Left err)
    Right jv -> FL.do
      (jv, jv2) <- Jq.copy jv
      L.Ur len <- Jq.arrayLength jv2

      let go arr ix acc
            | ix < 0 = FL.do
                Jq.free arr
                FL.pure (L.Ur $ Right acc)
            | otherwise = FL.do
                (arr, arr2) <- Jq.copy arr
                Jq.arrayGet arr2 ix FL.>>= \case
                  Left (L.Ur err) -> FL.do
                    Jq.free arr
                    FL.pure (L.Ur $ Left err)
                  Right v -> FL.do
                    runDecoder decoder v FL.>>= \case
                      L.Ur (Left err) -> FL.do
                        Jq.free arr
                        FL.pure (L.Ur $ Left err)
                      L.Ur (Right res) ->
                        go arr (ix - 1) (res : acc)

      go jv (len - 1) []
