{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Control.Functor.Linear as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.Functor.Linear as FL
import           Data.Unrestricted.Linear (Ur(..))
import qualified Prelude as P
import           Prelude.Linear
import qualified System.IO.Linear as L

import           Jq

main :: IO ()
main = L.withLinearIO P.$ L.do
  parse "{\"test\": true}" L.>>= FL.traverse cast L.>>= \case
    Left (Ur err) -> L.do
      L.fromSystemIO $ BS.putStrLn err
    Right Nothing -> L.do
      L.fromSystemIO $ BS.putStrLn "Wrong type!"
    Right (Just x) -> L.do
      (x, x') <- copy x
      str <- string "hello?"
      b <- bool False
      arr <- array [forgetType str, forgetType x, b]

      str2 <- string "foo"
      b2 <- bool True
      arr2 <- array [forgetType str2, b2]

      arr3 <- arrayConcat arr arr2

      n <- nullJv
      n2 <- nullJv

      arr4 <- arrayAppend arr3 n
      arr5 <- arrayAppend arr4 n2

      str2 <- string "testing"

      arr6 <- arraySet arr5 6 str2

      sliced <- arraySlice arr6 1 5

      obj <- object
      key <- string "key"
      obj2 <- objectSet obj key sliced
      obj3 <- objectMerge obj2 x'
      (obj3, obj3') <- copy obj3
      s <- string "bar"
      obj4 <- objectSet obj3 s obj3'
      (obj4, obj5) <- copy obj4

      fieldKey <- string "key"
      objectGet obj4 fieldKey L.>>= \case
        Nothing -> L.fromSystemIO $ BS.putStrLn "field not found"
        Just field -> L.do
          Ur fieldBs <- render field defPrintOpts
          L.fromSystemIO $ BS.putStrLn fieldBs

      Ur bs <- render obj5 defPrintOpts
        { printPretty = True, printColor = True, printSpace1 = True }

      L.fromSystemIO $ BS.putStrLn bs
  L.pure (Ur ())
