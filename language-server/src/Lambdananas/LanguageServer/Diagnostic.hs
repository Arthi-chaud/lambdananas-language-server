module Lambdananas.LanguageServer.Diagnostic (
    emitDiagnostics,
    loadCodingStyleWarnings,
    getAndEmitDiagnostics,
    loadAndEmitDiagnostics,
) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Lambdananas.LanguageServer.Logging
import Lambdananas.LanguageServer.Messages
import Lambdananas.LanguageServer.Monad
import Lambdananas.Wrapper (getCodingStyleWarnings)
import Lambdananas.Wrapper.Warn
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.FilePath
import Text.Printf (printf)

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
            Nothing -> loadAndEmitDiagnostics' filePath uri
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
        Just filePath -> loadAndEmitDiagnostics' filePath uri

-- Load and emit if in an applicable folder
--
-- If not applicable, diagnostics wont be loaded
loadAndEmitDiagnostics' :: FilePath -> Uri -> LSM ()
loadAndEmitDiagnostics' fp uri = do
    mrootPath <- getRootPath
    maybe
        loadAndEmit
        ( \rootPath ->
            -- Note: Not publishing if empty list might cinfuse client
            -- it might think it is still loading
            if isApplicableFolder rootPath fp
                then loadAndEmit
                else emitDiagnostics (toNormalizedUri uri) []
        )
        mrootPath
  where
    loadAndEmit =
        loadCodingStyleWarnings fp >>= emitDiagnostics (toNormalizedUri uri)
    isApplicableFolder rootPath filePath =
        let relPath = makeRelative rootPath filePath
         in -- We could have filtered by app and src, but what about pool days?
            not $ any (`isPrefixOf` relPath) ["test", "bonus/"]

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
    -- see https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
    let
        linePos = fromIntegral $ line warn - 1
        range =
            -- Note: lambdananas only gives us the line/row, not the column
            Range
                (Position linePos 0)
                -- might be safe, since CS imposes length of 80 max
                (Position linePos 100000)
        severity = Just $ getSeverity $ level warn
        getSeverity Major = DiagnosticSeverity_Error
        getSeverity Minor = DiagnosticSeverity_Warning
        getSeverity Info = DiagnosticSeverity_Information
        code = Just $ InR $ T.pack $ ruleCode warn
        codeDesc =
            Just $
                CodeDescription $
                    filePathToUri "https://intra.epitech.eu/file/Public/technical-documentations/Haskell/epitech_haskell_coding_style.pdf"
        src = Just $ T.pack "lambdananas"
        text = T.pack (printf "%s (%s)" (description warn) (show $ level warn))
        tags = Just []
        related = Nothing
        dataValue = Nothing
     in
        Diagnostic range severity code codeDesc src text tags related dataValue
