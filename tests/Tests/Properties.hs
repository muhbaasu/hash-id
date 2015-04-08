module Tests.Properties
       (
         tests
       ) where

import qualified Test.Framework                       as F
import qualified Test.Framework.Providers.QuickCheck2 as F
import           Test.QuickCheck

tests :: F.Test
tests =
  F.testGroup "Properties" []
