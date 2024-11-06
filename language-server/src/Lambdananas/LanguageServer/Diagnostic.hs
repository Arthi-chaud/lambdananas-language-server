module Lambdananas.LanguageServer.Diagnostic (emitDiagnostics) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Text as T
import Lambdananas.LanguageServer.Monad
import Lambdananas.Wrapper.Warn
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types
import Language.LSP.Server

emitDiagnostics :: NormalizedUri -> LSM ()
emitDiagnostics uri = do
    state <- lift ask >>= liftIO . takeMVar
    case uriToFilePath (fromNormalizedUri uri) >>= flip lookup state of
        Nothing -> return ()
        Just warns ->
            let diagnostics = partitionBySource $ warnToDiagnostic <$> warns
                maxDiags = 100
                version = Just 0
             in publishDiagnostics maxDiags uri version diagnostics

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
