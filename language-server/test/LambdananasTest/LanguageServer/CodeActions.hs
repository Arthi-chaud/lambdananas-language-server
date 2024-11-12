{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LambdananasTest.LanguageServer.CodeActions (specs) where

import Control.Monad.IO.Class
import Data.List (find)
import qualified Data.Text as T
import Language.LSP.Protocol.Message (SMethod (SMethod_TextDocumentDidSave))
import Language.LSP.Protocol.Types (CodeAction (..), Diagnostic (..), DidSaveTextDocumentParams (DidSaveTextDocumentParams), Position (Position), Range (Range), type (|?) (..))
import Language.LSP.Test
import System.FilePath
import Test.Hspec

specs :: Spec
specs =
    describe "Code Action" $ do
        it "should not suggest code actions" $
            runLLSSession $ do
                doc <- openDoc "./src/MyPutStr.hs" "haskell"
                _ <- waitForDiagnostics
                let range = Range (Position 2 0) (Position 2 0)
                ca <- getCodeActions doc range
                liftIO $ ca `shouldBe` []

        it "should suggest code action (insert header)" $
            runLLSSession $ do
                let src = "./src/MyPutStr.hs"
                    fullsrc = combine "./test/assets" src
                doc <- openDoc src "haskell"
                oldcontent <- documentContents doc
                diags <- waitForDiagnostics
                liftIO $ length diags `shouldBe` 3
                let range = Range (Position 0 0) (Position 0 0)
                actions <- getCodeActions doc range
                liftIO $ length actions `shouldBe` 1
                let InR action = head actions
                liftIO $ _title action `shouldBe` T.pack "Insert EPITECH Header"
                -- Executing an action does not actually write the src file
                -- So what we do is simulate a 'save' operation
                -- to trigger recomputation of diagnostics
                _ <- resolveAndExecuteCodeAction action
                content <- documentContents doc
                _ <- liftIO $ writeFile fullsrc $ T.unpack content
                _ <- sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams doc (Just content))
                newdiags <- waitForDiagnostics
                liftIO $ writeFile fullsrc $ T.unpack oldcontent
                liftIO $ do
                    length newdiags `shouldBe` length diags - 1
                    find (\d -> _code d == Just (InR "H-G1")) newdiags `shouldBe` Nothing
  where
    runLLSSession = runSession "lambdananas-language-server" fullCaps "./test/assets"
