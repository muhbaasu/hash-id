module Main (main) where

import           Test.Framework   (defaultMain)

import qualified Tests.Properties as Properties
import qualified Tests.Simple     as Simple

main :: IO ()
main = defaultMain [Properties.tests, Simple.tests]
