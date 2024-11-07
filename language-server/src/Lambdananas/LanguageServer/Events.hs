module Lambdananas.LanguageServer.Events (eventHandlers) where

import Lambdananas.LanguageServer.Events.Hover
import Lambdananas.LanguageServer.Events.Init
import Lambdananas.LanguageServer.Events.PullDiagnostics (onPullDiagnostics)
import Lambdananas.LanguageServer.Events.Save
import Lambdananas.LanguageServer.Events.TextDocumentSync
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Server (Handlers)

eventHandlers :: Handlers LSM
eventHandlers =
    mconcat
        [ onHover
        , onInit
        , onOpen
        , onChange
        , onClose
        , onSave
        , onPullDiagnostics
        ]
