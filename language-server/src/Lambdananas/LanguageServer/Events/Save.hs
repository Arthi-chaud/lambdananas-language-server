module Lambdananas.LanguageServer.Events.Save (onSave) where

import Control.Lens
import Lambdananas.LanguageServer.Diagnostic (loadAndEmitDiagnostics)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (
    SMethod (SMethod_TextDocumentDidSave),
 )
import Language.LSP.Server

onSave :: Handlers LSM
onSave = notificationHandler SMethod_TextDocumentDidSave $ \notif -> do
    debugLog "Received 'save' notification"
    let fileUri = notif ^. params . textDocument . uri
    loadAndEmitDiagnostics fileUri
