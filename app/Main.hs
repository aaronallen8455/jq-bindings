{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Control.Functor.Linear as L
import qualified Data.ByteString.Char8 as BS
import           Data.Unrestricted.Linear (Ur(..))
import qualified Prelude as P
import           Prelude.Linear
import qualified System.IO.Linear as L

import           Jq

main :: IO ()
main = L.withLinearIO P.$
  parse "{\"test\": true}" L.>>= \case
    Left (Ur err) -> L.do
      L.fromSystemIO $ BS.putStrLn err
      L.pure (Ur ())
    Right x -> L.do
      str <- string "hello?"
      b <- bool False
      arr <- array [forgetType str, x, b]

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
      Ur bs <- render (forgetType arr6) defPrintOpts
        { printPretty = True, printColor = True, printSpace1 = True }

      L.fromSystemIO $ BS.putStrLn bs
      L.pure (Ur ())
