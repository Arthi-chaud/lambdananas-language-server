{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdananasTest.LanguageServer.Diagnostics (specs) where

import Control.Monad.IO.Class
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Language.LSP.Protocol.Types (Diagnostic (..), DiagnosticSeverity (..), Position (..), Range (..), type (|?) (..))
import Language.LSP.Test
import Test.Hspec
import Text.Printf (printf)

specs :: Spec
specs =
    describe "Diagnostic Publication" $ do
        it "should publish diagnostics" $
            runLLSSession $ do
                _ <- openDoc "./src/MyPutStr.hs" "haskell"
                diags <- waitForDiagnostics
                liftIO $ do
                    length diags `shouldBe` 3
                    -- Header Warning
                    headerDiag <- getDiagnosticByCode "H-G1" diags
                    _severity headerDiag `shouldBe` Just DiagnosticSeverity_Warning
                    _source headerDiag `shouldBe` Just "lambdananas"
                    _message headerDiag
                        `shouldBe` "MyPutStr.hs has a badly formatted Epitech header (Minor)"
                    _range headerDiag
                        `shouldBe` expectedRange 0
                    -- Missing Signature
                    signWarn <- getDiagnosticByCode "H-T1" diags
                    _severity signWarn `shouldBe` Just DiagnosticSeverity_Error
                    _message signWarn
                        `shouldBe` "myPutStr has no signature (Major)"
                    _range signWarn
                        `shouldBe` expectedRange 2
                    -- Nested Ifs
                    ifsWarn <- getDiagnosticByCode "H-C4" diags
                    _severity ifsWarn `shouldBe` Just DiagnosticSeverity_Error
                    _message ifsWarn
                        `shouldBe` "nested ifs (Major)"
                    _range ifsWarn
                        `shouldBe` expectedRange 5

        it "should not publish diagnostics (test folder)" $
            runLLSSession $ do
                _ <- openDoc "./test/Specs.hs" "haskell"
                noDiagnostics

        it "should use state to reload diagnostics" $
            -- Note: We are not actually testing that it uses the state
            -- but we do test that opening the same file twice will work
            runLLSSession $ do
                _ <- openDoc "./src/MyPutStr.hs" "haskell"
                _ <- waitForDiagnostics
                _ <- openDoc "./test/Specs.hs" "haskell"
                _ <- waitForDiagnostics
                _ <- openDoc "./src/MyPutStr.hs" "haskell"
                diags <- waitForDiagnostics
                liftIO $ length diags `shouldBe` 3
  where
    expectedRange line = Range (Position line 0) (Position line 100000)
    getDiagnosticByCode code diags = case find (\d -> fromJust (_code d) == InR code) diags of
        Nothing -> fail $ printf "Diagnostic with code %s was not found" code
        Just diag -> return diag

    runLLSSession = runSession "lambdananas-language-server" fullCaps "./test/assets"
