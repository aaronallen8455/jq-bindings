{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Jq
  ( free
  , parse
  , copy
  , PrintOpts(..)
  , defPrintOpts
  , prettyPrint
  ) where

import qualified Control.Functor.Linear as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import           Data.Unrestricted.Linear (Ur(..))
import           Foreign.C.String
import qualified Prelude as P
import           Prelude.Linear
import qualified System.IO.Linear as L
import qualified Unsafe.Linear as UL

import           Jq.Internal.Bindings

free :: Jv %1 -> L.IO ()
free = L.fromSystemIO . UL.toLinear jvFree

parse :: BS.ByteString
      -> L.IO (Either (Ur BS.ByteString) Jv)
parse str = L.fromSystemIO $ do
  jv <- BS.useAsCString str jvParse
  validate jv

parseMaybe :: BS.ByteString -> L.IO (Maybe Jv)
parseMaybe str =
  either (\(Ur _) -> Nothing) Just L.<$> parse str

validate :: Jv -> IO (Either (Ur BS.ByteString) Jv)
validate jv = do
  k <- jvGetKind jv
  -- check if it's valid
  if k P./= jvKindInvalid
     then P.pure (Right jv)
     else do -- invalid
       msg <- jvInvalidGetMsg jv
       jvFree jv
       msgK <- jvGetKind msg
       -- check if there's an error message
       if msgK P.== jvKindNull
          then do
            jvFree msg
            P.pure (Left $ Ur "Invalid JSON")
          else do
            cStr <- jvStringValue msg
            jvFree msg
            bs <- BS.packCString cStr
            P.pure (Left $ Ur bs)

copy :: Jv %1 -> L.IO (Jv, Jv)
copy = UL.toLinear $ \jv -> L.do
  jv2 <- L.fromSystemIO (jvCopy jv)
  L.pure (jv, jv2)

data PrintOpts =
  MkPrintOpts
    { printPretty   :: Bool
    , printAscII    :: Bool
    , printColor    :: Bool
    , printSorted   :: Bool
    , printInvalid  :: Bool
    , printRefCount :: Bool
    , printTab      :: Bool
    , printIsatty   :: Bool
    , printSpace0   :: Bool
    , printSpace1   :: Bool
    , printSpace2   :: Bool
    } deriving Show

defPrintOpts :: PrintOpts
defPrintOpts =
  MkPrintOpts
    { printPretty   = False, printAscII    = False, printColor    = False
    , printSorted   = False, printInvalid  = False, printRefCount = False
    , printTab      = False, printIsatty   = False, printSpace0   = False
    , printSpace1   = False, printSpace2   = False
    }

printOptsToFlags :: PrintOpts -> JvPrintFlags
printOptsToFlags MkPrintOpts{..} =
  P.foldr (Bits..|.) 0 P.$ concat
    [ [ jvPrintPretty | printPretty ]
    , [ jvPrintAscII | printAscII ]
    , [ jvPrintColor | printColor ]
    , [ jvPrintSorted | printSorted ]
    , [ jvPrintInvalid | printInvalid ]
    , [ jvPrintRefCount | printRefCount ]
    , [ jvPrintTab | printTab ]
    , [ jvPrintIsatty | printIsatty ]
    , [ jvPrintSpace0 | printSpace0 ]
    , [ jvPrintSpace1 | printSpace1 ]
    , [ jvPrintSpace2 | printSpace2 ]
    ]

-- Consumes the Jv argument
prettyPrint :: Jv %1 -> PrintOpts -> L.IO (Ur BS.ByteString)
prettyPrint = UL.toLinear $ \jv opts -> L.fromSystemIO $ P.do
  x <- jvDumpString jv (printOptsToFlags opts)
  cStr <- jvStringValue x
  jvFree x
  bs <- BS.packCString cStr
  P.pure (Ur bs)
