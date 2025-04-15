module Lambdananas.LanguageServer.Events.PullDiagnostics (
    onPullDiagnostics,
) where

import Control.Lens
import Lambdananas.LanguageServer.Diagnostic (getAndEmitDiagnostics)
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Server

-- | Handler for 'Get Diagnostic' request
--
-- Sends back the saved diagnostics for the requested file
onPullDiagnostics :: Handlers LSM
onPullDiagnostics = requestHandler SMethod_TextDocumentDiagnostic $ \notif _ -> do
    debugLog "Received 'pull diagnostic' request"
    let fileUri = notif ^. params . textDocument . uri
    getAndEmitDiagnostics fileUri
