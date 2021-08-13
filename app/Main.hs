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
main = L.withLinearIO P.$ L.do
  parse "{\"test\": true}" L.>>= \case
    Left (Ur err) -> L.do
      L.fromSystemIO $ BS.putStrLn err
      L.pure (Ur ())
    Right x -> L.do
      str <- string "hello?"
      b <- bool False
      arr <- array [str, x, b]

      Ur bs <- render arr defPrintOpts
        { printPretty = True, printColor = True, printSpace1 = True }

      L.fromSystemIO $ BS.putStrLn bs
      L.pure (Ur ())

