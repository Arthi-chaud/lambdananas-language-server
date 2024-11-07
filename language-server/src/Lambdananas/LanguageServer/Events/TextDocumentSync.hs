module Lambdananas.LanguageServer.Events.TextDocumentSync (
    onOpen,
    onChange,
    onClose,
) where

import Control.Lens
import Lambdananas.LanguageServer.Diagnostic (getAndEmitDiagnostics)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Server

onOpen :: Handlers LSM
onOpen = notificationHandler SMethod_TextDocumentDidOpen $ \notif -> do
    debugLog "Received 'open' notification"
    let fileUri = notif ^. params . textDocument . uri
    getAndEmitDiagnostics fileUri

onChange :: Handlers LSM
onChange = notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
    debugLog "Received 'change' notification"
    return ()

onClose :: Handlers LSM
onClose = notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
    debugLog "Received 'close' notification"
    return ()
