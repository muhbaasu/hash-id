module Tests.Properties
       (
         tests
       ) where

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
     vectorOf k $ elements HI.defaultAlphabet

instance Arbitrary HI.HashEncoder where
  arbitrary = do
    salt <- arbitrary
    return $ HI.mkEncoder $ HI.defaultOptions salt

prop_idempotent_encode :: HI.HashEncoder -> [Int] -> Bool
prop_idempotent_encode enc nums = HI.encode enc nums == HI.encode enc nums

decodeReversesEncode :: HI.HashEncoder -> [Int] -> Bool
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
  ]
