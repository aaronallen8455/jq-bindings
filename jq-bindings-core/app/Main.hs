{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Control.Functor.Linear as L
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Functor.Linear as FL
import           Data.Unrestricted.Linear (Ur(..))
import           Foreign.C.String (withCString)
import qualified Prelude as P
import           Prelude.Linear
import qualified System.IO.Linear as L

import           Jq
import           Jq.Internal.Bindings

pgrm :: IO ()
pgrm = do
  s <- jqInit
  withCString ".test" P.$ jqCompile s
  jv <- withCString "{\"test\":21}" jvParse
  jqStart s jv 0
  jv' <- jqNext s
  jv'' <- jqNext s
  jvFree jv''
  jqTeardown s
  jvFree jv

  k <- jvGetKind jv'
  print k
  jvStr <- jvDumpString jv' 1
  cStr <- jvStringValue jvStr
  bs <- BSS.packCString cStr
  jvFree jvStr
  print bs

main :: IO ()
main = L.withLinearIO P.$ L.do
  parse "{\"test\": true}" L.>>= FL.traverse cast L.>>= \case
    Left (Ur err) -> L.do
      L.fromSystemIO $ BS.putStrLn err
    Right Nothing -> L.do
      L.fromSystemIO $ BS.putStrLn "Wrong type!"
    Right (Just x) -> L.do
      (x, x') <- copy x
      (x, x'') <- copy x
      Ur b <- equal x' x''
      L.fromSystemIO $ print b

      (x, x') <- copy x
      str <- string "hello?"
      b <- bool False
      arr <- array [forgetType str, forgetType x, forgetType b]

      str2 <- string "foo"
      b2 <- bool True
      arr2 <- array [forgetType str2, forgetType b2]

      arr3 <- arrayConcat arr arr2

      n <- nullJv
      n2 <- nullJv

      arr4 <- arrayAppend arr3 n
      arr5 <- arrayAppend arr4 n2

      str2 <- string "testing"

      arr6 <- arraySet arr5 6 str2

      sliced <- arraySlice arr6 1 5

      obj <- object
      obj2 <- objectSet obj "key" sliced
      obj3 <- objectMerge obj2 x'
      (obj3, obj3') <- copy obj3
      obj4 <- objectSet obj3 "bar" obj3'
      (obj4, obj5) <- copy obj4

      objectGet obj4 "key" L.>>= \case
        Nothing -> L.fromSystemIO $ BS.putStrLn "field not found"
        Just field -> L.do
          Ur fieldBs <- render field defPrintOpts
          L.fromSystemIO $ BS.putStrLn fieldBs

      (obj5, obj6) <- copy obj5
      Ur bs <- render obj5 defPrintOpts
        { printPretty = True, printColor = True, printSpace1 = True }

      L.fromSystemIO $ BS.putStrLn bs

      (obj6, obj) <- copy obj6
      --pgrmResults <- execProgramUnsafe "..[ \"test\"  ].foo.zoo[   1 , 3 ]" obj
      pgrmResults <- execProgram [jq|..["test"] | .|] obj

      (bss :: [Ur BS.ByteString]) <-
        FL.traverse (flip render defPrintOpts { printPretty = True, printSpace1 = True })
          pgrmResults
      FL.void $ FL.for bss P.$
        \(Ur bs) -> L.fromSystemIO (BS.putStrLn bs)

      (obj6, obj7) <- copy obj6

      getPath (forgetType obj6) ["key", 0, "test"] L.>>= \case
        Just thing -> L.do
          Ur bs2 <- render thing defPrintOpts
          L.fromSystemIO $ BS.putStrLn bs2
        Nothing -> L.fromSystemIO $ BS.putStrLn "doesn't exist"

      bool False L.>>= setPath (forgetType obj7) ["key", 0, "test"] L.>>= \case
        Just obj7 -> L.do
          Ur bs <- render obj7 defPrintOpts
            { printPretty = True, printColor = True, printSpace1 = True }
          L.fromSystemIO $ BS.putStrLn bs
        Nothing -> L.pure ()

  L.pure (Ur ())
