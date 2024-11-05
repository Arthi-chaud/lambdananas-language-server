module LambdananasTest.Wrapper.Warn (specs) where

import Lambdananas.Wrapper.Warn (
    CodingStyleWarning (..),
    SeverityLevel (..),
    parseCodingStyleWarning,
 )
import Test.Hspec
import Text.Megaparsec (parse)

specs :: Spec
specs =
    describe "Parse Coding Style Mistake" $ do
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
        it "should parse 'forbidden extension' message" $
            testParse
                "./src/Lambdananas/Wrapper.hs contains forbidden extensions"
                CodingStyleWarning
                    { line = 1
                    , level = Major
                    , fileName = "./src/Lambdananas/Wrapper.hs"
                    , ruleCode = "H-E1"
                    , description = "language extensions are forbidden"
                    }
  where
    testParse str obj = case parse parseCodingStyleWarning "" str of
        Left err -> expectationFailure $ show err
        Right parsed -> parsed `shouldBe` obj
