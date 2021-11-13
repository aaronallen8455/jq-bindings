{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Functor.Linear as FL
import qualified Data.ByteString.Char8 as BS8
import           Data.Coerce (coerce)
import           Data.Functor.Contravariant
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Jq.Decode as Dec
import qualified Jq.Encode as Enc
import qualified System.IO.Linear as L
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $
  testGroup "roundtrip tests"
    [ tripAll "int" Enc.int Dec.int
    , tripAll "double" Enc.double Dec.double
    , tripAll "string" (unJsonString >$< Enc.string)
                       (JsonString <$> Dec.string)
    , tripAll "byteString" (BS8.pack . unJsonString >$< Enc.byteString)
                           (JsonString . BS8.unpack <$> Dec.byteString)
    , tripAll "text" (T.pack . unJsonString >$< Enc.text)
                     (JsonString . T.unpack <$> Dec.text)
    , tripAll "bool" Enc.bool Dec.bool
    , tripAll "object" testObjEnc testObjDec
    ]

tripAll :: (Arbitrary a, Eq a, Show a)
        => String -> Enc.Encoder a -> Dec.Decoder a -> TestTree
tripAll name enc dec =
  testGroup name
    [ testProperty "plain" (trip enc dec)
    , testProperty "nullable" (trip (Enc.nullable enc) (Dec.nullable dec))
    , testProperty "array" (trip (Enc.array enc) (Dec.array dec))
    ]

trip :: (Arbitrary a, Eq a, Show a)
     => Enc.Encoder a -> Dec.Decoder a -> Property
trip enc dec = do
  forAll arbitrary $ \i -> ioProperty $ do
    tripped <- L.withLinearIO $
      Enc.runEncoder enc i FL.>>= Dec.runDecoder dec
    pure $ Right i === tripped

data TestObj =
  TestObj
    { field1 :: Bool
    , field2 :: TestObj2
    , field3 :: Maybe JsonString
    } deriving (Eq, Show)

testObjDec :: Dec.Decoder TestObj
testObjDec =
  TestObj
  <$> Dec.requiredKey "field1" Dec.bool
  <*> Dec.requiredKey "field2" testObj2Dec
  <*> Dec.optionalKey "field3" (JsonString <$> Dec.string)

testObjEnc :: Enc.Encoder TestObj
testObjEnc = Enc.object
  [ Enc.objField "field1" field1 Enc.bool
  , Enc.objField "field2" field2 testObj2Enc
  , Enc.objFieldOmitNull "field3" field3 (unJsonString >$< Enc.string)
  ]

instance Arbitrary TestObj where
  arbitrary =
    TestObj
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

data TestObj2 =
  TestObj2
    { field21 :: Int
    , field22 :: [Double]
    , field23 :: Maybe JsonString
    } deriving (Eq, Show)

testObj2Dec :: Dec.Decoder TestObj2
testObj2Dec =
  TestObj2
  <$> Dec.requiredKey "field21" Dec.int
  <*> Dec.requiredKey "field22" (Dec.array Dec.double)
  <*> Dec.nullableKey "field23" (JsonString <$> Dec.string)

testObj2Enc :: Enc.Encoder TestObj2
testObj2Enc = Enc.object
  [ Enc.objField "field21" field21 Enc.int
  , Enc.objField "field22" field22 (Enc.array Enc.double)
  , Enc.objField "field23" field23 (Enc.nullable (unJsonString >$< Enc.string))
  ]

instance Arbitrary TestObj2 where
  arbitrary =
    TestObj2
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

newtype JsonString =
  JsonString { unJsonString :: String }
  deriving (Eq, Show)

instance Arbitrary JsonString where
  arbitrary = JsonString . filter (/= '\NUL')
            . getASCIIString <$> arbitrary
  shrink = fmap (JsonString . getASCIIString)
         . shrink . ASCIIString . unJsonString
