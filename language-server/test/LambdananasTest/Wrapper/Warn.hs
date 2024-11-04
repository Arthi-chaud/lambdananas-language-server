module LambdananasTest.Wrapper.Warn (specs) where

import Lambdananas.Wrapper.Warn (
    CodingStyleWarning (..),
    SeverityLevel (Minor),
    parseCodingStyleWarning,
 )
import Test.Hspec
import Text.Megaparsec (parse)

specs :: Spec
specs =
    describe "Parse Coding Style Mistake" $
        it "should parse" $
            testParse
                "./app/Main.hs:73: MINOR:H-F3 # too long line"
                CodingStyleWarning
                    { line = 73
                    , level = Minor
                    , fileName = "./app/Main.hs"
                    , ruleCode = "H-F3"
                    , description = "too long line"
                    }
  where
    testParse str obj = case parse parseCodingStyleWarning "" str of
        Left err -> expectationFailure $ show err
        Right parsed -> parsed `shouldBe` obj
