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
        it "should parse bogus 'could not parse' message" $
            testParse
                " contains forbidden extensions"
                CodingStyleWarning
                    { line = 1
                    , level = Major
                    , fileName = ""
                    , ruleCode = "H-E1"
                    , description = "Lambdananas could not parse this file because of a non-standard syntax"
                    }
        it "should remove filepath in description" $
            testParse
                "src/Lambdananas/LanguageServer/Events.hs:1: MINOR:H-G1 # src/Lambdananas/LanguageServer/Events.hs has a badly formatted Epitech header"
                CodingStyleWarning
                    { line = 1
                    , level = Minor
                    , fileName = "src/Lambdananas/LanguageServer/Events.hs"
                    , ruleCode = "H-G1"
                    , description = "Events.hs has a badly formatted Epitech header"
                    }
  where
    testParse str obj = case parse (parseCodingStyleWarning "") "" str of
        Left err -> expectationFailure $ show err
        Right parsed -> parsed `shouldBe` obj
