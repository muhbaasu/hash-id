module Tests.Properties
       (
         tests
       ) where

import qualified Data.HashId.Internal                 as HI
import qualified Test.Framework                       as F
import qualified Test.Framework.Providers.QuickCheck2 as F
import           Test.QuickCheck

instance Arbitrary HI.HashEncoder where
  arbitrary = do
   salt <- arbitrary
   return $ HI.mkEncoder $ HI.defaultOptions salt

prop_idempotent_encode :: HI.HashEncoder -> [Int] -> Bool
prop_idempotent_encode enc nums = HI.encode enc nums == HI.encode enc nums

tests :: F.Test
tests =
  F.testGroup "Properties"
  [ F.testProperty "Idempotent encode" prop_idempotent_encode
  ]
