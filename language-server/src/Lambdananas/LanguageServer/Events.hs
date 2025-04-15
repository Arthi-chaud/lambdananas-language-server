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
        , onHover
        ]

onHover :: Handlers LSM
onHover = requestHandler SMethod_TextDocumentHover $ \req responder -> do
    let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
        Position _l _c' = pos
        rsp = Hover (InL ms) (Just range)
        ms = mkMarkdown $ pack "See you at Lambda Days 2025 👋\n\n\n\n![img](/Users/arthur/Downloads/hello-penguin.gif)"
        range = Range pos pos
    responder (Right $ InL rsp)
