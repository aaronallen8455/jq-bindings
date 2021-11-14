{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
module Main where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Functor.Linear as FL
import           Control.Monad.IO.Class.Linear (liftSystemIO, liftSystemIOU)
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Functor.Linear as DL
import qualified Prelude.Linear as L
import qualified System.IO.Linear as L

import           Jq

main :: IO ()
main = do
  bss <- L.withLinearIO $ FL.do
    jv <- loadFile "./persons9000.json"
    typeJv jv FL.>>= \case
      Left (L.Ur err) -> FL.do
        L.fromSystemIO (print err)
        FL.pure (L.Ur [])
      Right jv -> FL.do
        L.fromSystemIO (putStrLn "RIGHT")
        results <- execProgram
          [jq| .[0][]
            | select((.eyeColor == "green" or .eyeColor == "blue")
                and .age == 30 and (.tags[] | . == "sit" or . == "nisi"))
            | { eyeColor, age, isActive, balance, name, tags }
          |] jv

        renderMultiple results defPrintOpts
          { printPretty = True
          , printSpace1 = True
          , printColor = True
          , printInvalid = True
          }
  print (length bss)
  traverse_ BS.putStrLn bss
