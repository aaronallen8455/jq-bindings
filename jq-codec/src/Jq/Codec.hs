{-# LANGUAGE RankNTypes #-}
module Jq.Codec
  ( Decoder(..)
  , Encoder(..)
  ) where

import qualified Data.ByteString as BS
import qualified Control.Monad.IO.Class.Linear as L
import qualified Prelude.Linear as L

import           Jq

newtype Decoder a =
  MkDecoder
    { runDecoder :: forall jv m. (HasJv jv, L.MonadIO m)
                 => jv -> m (Either BS.ByteString a)
    }

newtype Encoder a =
  MkEncoder
    { runEncoder :: forall m. L.MonadIO m => a -> m SomeTypedJv }
