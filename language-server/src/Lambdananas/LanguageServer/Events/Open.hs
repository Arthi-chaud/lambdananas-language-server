module Lambdananas.LanguageServer.Events.Open (onOpen) where

import Control.Lens
import Lambdananas.LanguageServer.Diagnostic (loadAndEmitDiagnostics)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (
    SMethod (SMethod_TextDocumentDidOpen),
 )
import Language.LSP.Server

onOpen :: Handlers LSM
onOpen = notificationHandler SMethod_TextDocumentDidOpen $ \notif -> do
    debugLog "Received 'open' notification"
    let fileUri = notif ^. params . textDocument . uri
    loadAndEmitDiagnostics fileUri
