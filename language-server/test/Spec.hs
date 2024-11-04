module Main (main) where

import qualified LambdananasTest.Wrapper.Warn as Warn
import Test.Hspec

main :: IO ()
main = hspec $ do
    Warn.specs
