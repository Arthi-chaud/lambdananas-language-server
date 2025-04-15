module Lambdananas.LanguageServer.Events.FileEvents (
    onDelete,
    onFileEvent,
    onRename,
) where

import Control.Lens
import Control.Monad
import Lambdananas.LanguageServer.Diagnostic
import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad (LSM, withState)
import Lambdananas.LanguageServer.State (moveDiagnostics, removeDiagnsotics)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server

-- | Callback when a change has been detected for a file/folder in the IDE workspace
onFileEvent :: Handlers LSM
onFileEvent = notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \notif -> do
    debugLog "Received 'workspace changed' notification"
    let events = notif ^. params . changes
    forM_ events $ \e -> do
        let eventUri = e ^. uri
        let removeUriFromState u = do
                withState (\s -> ((), removeDiagnsotics u s))
                emitDiagnostics (toNormalizedUri u) []
        case e ^. type_ of
            FileChangeType_Deleted -> removeUriFromState eventUri
            -- TODO: don't remove, just reload diagnostics?
            FileChangeType_Changed -> removeUriFromState eventUri
            _ -> return ()

-- | Callback on file deletion
--
-- Removes the diagnostic for the given file from the state
onDelete :: Handlers LSM
onDelete = notificationHandler SMethod_WorkspaceDidDeleteFiles $ \notif -> do
    debugLog "Received 'file deletion' notification"
    let deletedUris = Uri . (^. uri) <$> notif ^. params . files
    withState (\state -> ((), foldr removeDiagnsotics state deletedUris))
    -- Flushing diagnostics for the old uri
    forM_ deletedUris (\u -> emitDiagnostics (toNormalizedUri u) [])

-- | Callback on file renaming
--
-- Renames the file in the state
onRename :: Handlers LSM
onRename = notificationHandler SMethod_WorkspaceDidRenameFiles $ \notif -> do
    debugLog "Received 'file rename' notification"
    let renamedUris = (\(FileRename old new) -> (Uri old, Uri new)) <$> notif ^. params . files
    withState (\state -> ((), foldr (uncurry moveDiagnostics) state renamedUris))
    -- Flushing diagnostics for the old uris
    forM_ renamedUris (\(u, _) -> emitDiagnostics (toNormalizedUri u) [])
