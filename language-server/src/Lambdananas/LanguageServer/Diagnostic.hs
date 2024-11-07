module Lambdananas.LanguageServer.Diagnostic (
    emitDiagnostics,
    loadCodingStyleWarnings,
    getAndEmitDiagnostics,
    loadAndEmitDiagnostics,
) where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Text as T
import Lambdananas.LanguageServer.Logging
import Lambdananas.LanguageServer.Messages
import Lambdananas.LanguageServer.Monad
import Lambdananas.Wrapper (getCodingStyleWarnings)
import Lambdananas.Wrapper.Warn
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types
import Language.LSP.Server

-- | Load warnings (only if it is not already in state) for the file at the given uri, saves them in the state,
-- and emit them.
getAndEmitDiagnostics :: Uri -> LSM ()
getAndEmitDiagnostics uri = do
    state <- getState
    case uriToFilePath uri of
        Nothing -> do
            let errMsg = "Could not get file path from uri"
            errorLog errMsg
            sendErrorMessage errMsg
        Just filePath -> case lookup filePath state of
            Nothing -> do
                warns <- loadCodingStyleWarnings filePath
                emitDiagnostics (toNormalizedUri uri) warns
            Just warns -> emitDiagnostics (toNormalizedUri uri) warns

-- | Load warnings (even if it is already in state) for the file at the given uri, saves them in the state,
-- and emit them.
loadAndEmitDiagnostics :: Uri -> LSM ()
loadAndEmitDiagnostics uri =
    case uriToFilePath uri of
        Nothing -> do
            let errMsg = "Could not get file path from uri"
            errorLog errMsg
            sendErrorMessage errMsg
        Just filePath -> do
            warns <- loadCodingStyleWarnings filePath
            emitDiagnostics (toNormalizedUri uri) warns

-- | Calls lambdananas, update state with new warnings, and return them
loadCodingStyleWarnings :: FilePath -> LSM [CodingStyleWarning]
loadCodingStyleWarnings filePath = do
    res <- liftIO $ runExceptT $ do
        warnlist <- ExceptT $ getCodingStyleWarnings filePath
        return $ case warnlist of
            (warns : _) -> snd warns
            [] -> []
    case res of
        Left err -> sendErrorMessage (show err) >> return []
        Right warns -> do
            state <- getState
            let cleanState = filter (\(fp, _) -> fp /= filePath) state
                newState = (filePath, warns) : cleanState
            setState newState
            return warns

-- | Publish diagnostics for the given file
emitDiagnostics :: NormalizedUri -> [CodingStyleWarning] -> LSM ()
emitDiagnostics uri warns =
    let diagnostics = partitionBySource $ warnToDiagnostic <$> warns
        maxDiags = 100
        version = Just 0
     in publishDiagnostics maxDiags uri version diagnostics

-- | Turn a 'CodingStyleWarning' on an LSP 'Diagnostic' model
warnToDiagnostic :: CodingStyleWarning -> Diagnostic
warnToDiagnostic warn =
    let
        linePos = (fromIntegral $ line warn - 1)
        range = Range (Position linePos 0) (Position linePos 100)
        severity = Just $ getSeverity $ level warn
        getSeverity Major = DiagnosticSeverity_Error
        getSeverity Minor = DiagnosticSeverity_Warning
        getSeverity Info = DiagnosticSeverity_Information
        code = Nothing
        codeDesc = Nothing
        src = Nothing
        text = T.pack (description warn)
        tags = Just []
        related = Nothing
        dataValue = Nothing
     in
        Diagnostic range severity code codeDesc src text tags related dataValue
