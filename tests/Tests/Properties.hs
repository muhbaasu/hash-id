{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Properties
       (
         tests
       ) where

import           Data.Either                          (isLeft, isRight)
import qualified Data.HashId.Internal                 as HI
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

instance Arbitrary HI.Positive where
  arbitrary = do
    (Positive pos) <- positiveIntGen
    return $ HI.Positive pos

prop_idempotent_encode :: HI.HashEncoder -> [HI.Positive] -> Bool
prop_idempotent_encode enc nums = HI.encode enc nums == HI.encode enc nums

prop_positive :: Int -> Bool
prop_positive n | n < 0 = isLeft $ HI.positive n
prop_positive n = isRight $ HI.positive n

decodeReversesEncode :: HI.HashEncoder -> [HI.Positive] -> Bool
decodeReversesEncode enc nums = let encoded = HI.encode enc nums
                                    decoded = HI.decode enc encoded
                                in decoded == nums

unhashReversesHash :: HI.Alphabet -> Int -> Bool
unhashReversesHash alphabet num = let hashed = HI.hash alphabet num
                                  in HI.unhash alphabet hashed == num

tests :: F.Test
tests =
  F.testGroup "Properties"
  [ F.testProperty "Idempotent encode" prop_idempotent_encode
  , F.testProperty "decode reverses encode" decodeReversesEncode
  , F.testProperty "unhash reverses hash" unhashReversesHash
  , F.testProperty "validate positive" prop_positive
  ]
