module Main (main) where

import qualified LambdananasTest.LanguageServer.CodeActions as CodeActions
import qualified LambdananasTest.LanguageServer.Diagnostics as Diagnostics
import qualified LambdananasTest.Wrapper.Warn as Warn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Wrapper" Warn.specs
    describe "LanguageServer" $ do
        Diagnostics.specs
        CodeActions.specs
