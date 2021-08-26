{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jq.Aeson
  ( toAesonValue
  , fromAesonValue
  ) where

import qualified Control.Functor.Linear as L
import qualified Control.Monad.IO.Class.Linear as L
import           Data.Aeson as Aeson
import           Data.Foldable (toList)
import qualified Data.Functor.Linear as L hiding (pure, (<$>))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Unrestricted.Linear (Ur(..))
import qualified Prelude.Linear as L

import           Jq as Jq

toAesonValue :: forall m. L.MonadIO m => SomeTypedJv %1 -> m (Ur Value)
toAesonValue sjv =
  withSomeTypedJv sjv $ \jv -> \case
    NullS -> L.do
      free jv
      L.pure (Ur Null)

    FalseS -> L.do
      free jv
      L.pure (Ur (Bool False))

    TrueS -> L.do
      free jv
      L.pure (Ur (Bool True))

    NumberS -> L.do
      (jv, Ur n) <- numberValue jv
      free jv
      L.pure (Ur (Number (realToFrac n)))

    StringS -> L.do
      (jv, Ur bs) <- stringValue jv
      free jv
      L.pure (Ur (String (TE.decodeUtf8 bs)))

    ArrayS -> L.do
      (jv, jv2) <- copy jv
      (Ur len) <- arrayLength jv

      let getElems :: (TypedJv 'ArrayKind, Ur [Value]) %1
                   -> Ur Int %1
                   -> m (TypedJv 'ArrayKind, Ur [Value])
          getElems (jv, Ur els) (Ur i) = L.do
            (jv, jv2) <- copy jv

            Ur v <- arrayGet jv i L.>>= \case
                      Nothing -> L.pure (Ur Null)
                      Just sjv -> toAesonValue sjv

            L.pure (jv2, Ur (v : els))

      (jv, Ur es) <- L.foldM getElems (jv2, Ur []) L.$
                       Ur <$> [len - 1, len - 2 .. 0]

      free jv

      L.pure (Ur $ toJSON es)

    ObjectS -> L.do
      (jv, jv2) <- copy jv
      keys <- objectKeys jv

      let getPairs :: (TypedJv 'ObjectKind, Ur [(Text, Value)]) %1
                   -> TypedJv 'StringKind %1
                   -> m (TypedJv 'ObjectKind, Ur [(Text, Value)])
          getPairs (jv, Ur pairs) key = L.do
            (key, Ur keyBs) <- stringValue key
            free key
            (jv, jv2) <- copy jv

            Ur pair <- objectGet jv keyBs L.>>= \case
              Nothing -> L.pure (Ur (TE.decodeUtf8 keyBs, Null))
              Just sjv -> L.do
                Ur v <- toAesonValue sjv
                L.pure (Ur (TE.decodeUtf8 keyBs, v))

            L.pure (jv2, Ur (pair : pairs))

      (jv, Ur pairs) <- L.foldM getPairs (jv2, Ur []) keys

      free jv

      L.pure (Ur $ Aeson.object pairs)

fromAesonValue :: forall m. L.MonadIO m => Ur Value %1 -> m SomeTypedJv
fromAesonValue (Ur v) = case v of
  Null -> MkSomeTypedJv NullS L.<$> nullJv

  Bool b -> bool b

  Number n -> MkSomeTypedJv NumberS L.<$> number n

  String txt -> MkSomeTypedJv StringS L.<$> string (TE.encodeUtf8 txt)

  Array vec -> L.do
    jv <- L.traverse fromAesonValue (Ur <$> toList vec)
      L.>>= array
    L.pure (MkSomeTypedJv ArrayS jv)

  Object hashMap -> L.do
    obj <- Jq.object

    let setKey :: TypedJv 'ObjectKind %1
               -> Ur (Text, Value) %1
               -> m (TypedJv 'ObjectKind)
        setKey obj (Ur (k, v)) = L.do
          jv <- fromAesonValue (Ur v)
          objectSet obj (TE.encodeUtf8 k) jv

    obj <- L.foldM setKey obj (Ur <$> HM.toList hashMap)
    L.pure (MkSomeTypedJv ObjectS obj)
