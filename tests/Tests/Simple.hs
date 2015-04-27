module Tests.Simple
       (
         tests
       ) where

import qualified Data.HashId.Internal           as HI
import           Data.Maybe                     (fromJust)
import qualified Test.Framework                 as F
import qualified Test.Framework.Providers.HUnit as F
import           Test.HUnit

consistentShuffleABC1 :: Assertion
consistentShuffleABC1 =
  s @=? shuffled
  where s = "cUpI6isqCa0brWZnJA8wNTzDHEtLXOYgh5fQm2uRj4deM91oB7FkSGKxvyVP3l"
        shuffled = HI.consistentShuffle
                   (HI.unAlpha HI.defaultAlphabet) $ HI.Salt "abc1"

consistentShuffleABC2 :: Assertion
consistentShuffleABC2 =
  s @=? shuffled
  where s = "tRvkhHx0ZefcF46YuaAqGLDKgM1W5Vp2T8n9s7BSoCjiQOdrEbJmUINywzXP3l"
        shuffled = HI.consistentShuffle
                   (HI.unAlpha HI.defaultAlphabet) $ HI.Salt "abc2"

encodeNumber :: Assertion
encodeNumber = let options = HI.defaultOptions $ HI.Salt "this is my salt"
                   encoder = HI.mkEncoder options
                   hid = HI.encode encoder [12345]
                   expected = "NkK9"
                   result = HI.toText hid
               in expected @=? result

decodeNumber :: Assertion
decodeNumber = let options = HI.defaultOptions $ HI.Salt "this is my salt"
                   encoder = HI.mkEncoder options
                   hid = fromJust $ HI.parse encoder "NkK9"
                   expected = [12345]
                   result = HI.decode encoder hid
               in expected @=? result

tests :: F.Test
tests =
  F.testGroup "Simple"
  [ F.testCase "Encode number" encodeNumber
  , F.testCase "Decode number" decodeNumber
  , F.testCase "Shuffle with salt abc1" consistentShuffleABC1
  , F.testCase "Shuffle with salt abc2" consistentShuffleABC2 ]
