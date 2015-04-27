{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Properties
       (
         tests
       ) where

import qualified Data.HashId.Internal                 as HI
import           Numeric.Natural
import qualified Test.Framework                       as F
import qualified Test.Framework.Providers.QuickCheck2 as F
import           Test.QuickCheck

instance Arbitrary HI.Salt where
  arbitrary = do
    salt' <- saltGen 4
    return $ HI.Salt salt'

saltGen :: Int -> Gen String
saltGen minSize = sized $ \n ->
  do k <- choose (minSize, minSize `max` n)
     vectorOf k $ elements $ HI.unAlpha $ HI.defaultAlphabet

instance Arbitrary HI.HashEncoder where
  arbitrary = do
    salt <- arbitrary
    return $ HI.mkEncoder $ HI.defaultOptions salt

instance Arbitrary HI.Alphabet where
  arbitrary = return HI.defaultAlphabet

positiveIntGen :: Gen (Positive Int)
positiveIntGen = arbitrary

instance Arbitrary Natural where
  arbitrary = do
    (Positive pos) <- positiveIntGen
    return $ fromIntegral pos

prop_idempotent_encode :: HI.HashEncoder -> [Natural] -> Bool
prop_idempotent_encode enc nums = HI.encode enc nums == HI.encode enc nums

decodeReversesEncode :: HI.HashEncoder -> [Natural] -> Bool
decodeReversesEncode enc nums = let encoded = HI.encode enc nums
                                    decoded = HI.decode enc encoded
                                in decoded == nums

unhashReversesHash :: HI.Alphabet -> Natural -> Bool
unhashReversesHash alphabet num = let hashed = HI.hash alphabet num
                                  in HI.unhash alphabet hashed == num

tests :: F.Test
tests =
  F.testGroup "Properties"
  [ F.testProperty "Idempotent encode" prop_idempotent_encode
  , F.testProperty "decode reverses encode" decodeReversesEncode
  , F.testProperty "unhash reverses hash" unhashReversesHash
  ]
