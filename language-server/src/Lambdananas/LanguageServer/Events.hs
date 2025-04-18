module Lambdananas.LanguageServer.Events (eventHandlers) where

import Data.Text (pack)
import Lambdananas.LanguageServer.Events.CodeAction (onCodeActionRequest)
import Lambdananas.LanguageServer.Events.FileEvents (onDelete, onFileEvent, onRename)
import Lambdananas.LanguageServer.Events.Init
import Lambdananas.LanguageServer.Events.PullDiagnostics (onPullDiagnostics)
import Lambdananas.LanguageServer.Events.Save
import Lambdananas.LanguageServer.Events.TextDocumentSync
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

eventHandlers :: Handlers LSM
eventHandlers =
    mconcat
        [ onInit
        , onTextDocumentEvent
        , onSave
        , onPullDiagnostics
        , onCodeActionRequest
        , onFileEvent
        , onDelete
        , onRename
        ]
