module Lambdananas.LanguageServer.Events.TextDocumentSync (
    onTextDocumentEvent,
) where

import Control.Lens
import Lambdananas.LanguageServer.Diagnostic (getAndEmitDiagnostics)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Server

onTextDocumentEvent :: Handlers LSM
onTextDocumentEvent = mconcat [onOpen, onChange, onClose]

-- | Callback on file opeing
--
-- Loads the diagnostics into the state and publishs them
onOpen :: Handlers LSM
onOpen = notificationHandler SMethod_TextDocumentDidOpen $ \notif -> do
    debugLog "Received 'open' notification"
    let fileUri = notif ^. params . textDocument . uri
    getAndEmitDiagnostics fileUri

-- | Callback on file change
--
-- Currently no-op
onChange :: Handlers LSM
onChange = notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
    debugLog "Received 'change' notification"
    return ()

-- | Callback on file closing
--
-- Currently no-op
onClose :: Handlers LSM
onClose = notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
    debugLog "Received 'close' notification"
    return ()
