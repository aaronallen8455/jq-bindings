{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Control.Functor.Linear as L
import           Data.Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import qualified Prelude.Linear as L
import qualified System.IO.Linear as L
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Jq.Aeson

main :: IO ()
main = defaultMain $
  testGroup "tests"
    [ testProperty "roundTrip" (withMaxSuccess 10000 roundTrip) ]

roundTrip :: Property
roundTrip =
  forAll arbitrary $ \value -> ioProperty $ do
    tripped <- L.withLinearIO $
      fromAesonValue (L.Ur value) L.>>= toAesonValue
    pure $ value === tripped

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, string, number]
  | otherwise = resize (div n 2)
      $ oneof [pure Null, bool, string, number, object', array]
  where
    bool = Bool <$> arbitrary
    string = String . unJsonKey <$> arbitrary
    -- numeric round tripping is fragile due to floating point conversion
    number = Number . (/100000) . fromIntegral <$> chooseInt (0, 999999999)
    object' = Object . HM.mapKeys unJsonKey <$> arbitrary
    array = Array <$> arbitrary

newtype JsonKey =
  JsonKey { unJsonKey :: T.Text }
  deriving (Eq, Hashable)

instance Arbitrary JsonKey where
  arbitrary = JsonKey . T.pack <$> liftArbitrary objKeyCharGen

objKeyCharGen :: Gen Char
objKeyCharGen = elements $
  ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_', '$', '!']
