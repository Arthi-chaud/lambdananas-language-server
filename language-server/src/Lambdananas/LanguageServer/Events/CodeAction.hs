{-# LANGUAGE RecordWildCards #-}

module Lambdananas.LanguageServer.Events.CodeAction (onCodeActionRequest) where

import Control.Lens
import Control.Monad.IO.Class
import Data.List (find)
import Data.Map
import Data.Maybe
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Lambdananas.LanguageServer.Diagnostic (warnToDiagnostic)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM, getCodingStyleWarningForFile)
import qualified Lambdananas.Wrapper.Warn as W
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (SMethod (SMethod_TextDocumentCodeAction))
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.FilePath (takeBaseName)

onCodeActionRequest :: Handlers LSM
onCodeActionRequest = requestHandler SMethod_TextDocumentCodeAction $ \req responder -> do
    debugLog "Code action request"
    let CodeActionParams _ _ doc r _ = req ^. params
        Range (Position warnLine _) _ = r
        fileUri = doc ^. uri
    warns <- getCodingStyleWarningForFile fileUri
    debugLog $ show warns
    case find (\w -> (W.line w - 1) == fromIntegral warnLine) warns of
        Nothing -> responder $ Right (InR Null)
        Just warn -> case W.ruleCode warn of
            "H-G1" -> do
                action <- liftIO $ insertHeaderAction (warnToDiagnostic warn) fileUri
                responder $ Right (InL [InR action])
            _ -> responder $ Right (InR Null)

insertHeaderAction :: Diagnostic -> Uri -> IO CodeAction
insertHeaderAction diag fileUri = do
    year <- getCurrentTime <&> (\t -> let (year, _, _) = toGregorian $ utctDay t in year)

    let
        filePath = fromMaybe "" $ uriToFilePath fileUri
        _title = T.pack "Insert EPITECH Header"
        _kind = Just CodeActionKind_QuickFix
        _diagnostics = Just [diag]
        _isPreferred = Just True
        _disabled = Nothing
        _command = Nothing
        _data_ = Nothing
        _edit =
            Just $
                let
                    headerPos = Position 0 0
                    editRange = Range headerPos headerPos
                 in
                    WorkspaceEdit
                        { _changes = Just $ singleton fileUri [TextEdit editRange (T.pack $ epitechHeader year filePath)]
                        , _documentChanges = Nothing
                        , _changeAnnotations = Nothing
                        }
     in
        return $ CodeAction{..}

epitechHeader :: Integer -> FilePath -> String
epitechHeader year fileName =
    unlines
        [ "{-"
        , "-- EPITECH PROJECT, " ++ show year
        , "-- " ++ takeBaseName fileName
        , "-- File description:"
        , "-- "
        , "-}"
        ]
