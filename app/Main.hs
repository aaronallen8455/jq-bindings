{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ep <- parse "{\"test\": true}"
  -- case statement doesn't work :(
  either
    (\(Ur err) -> L.do
      L.fromSystemIO $ BS.putStrLn err
      L.pure (Ur ())
    )
    (\x -> L.do
      Ur bs <- prettyPrint x defPrintOpts
        { printPretty = True, printColor = True }
      L.fromSystemIO $ BS.putStrLn bs
      L.pure (Ur ())
    ) ep

