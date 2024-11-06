module Lambdananas.LanguageServer.Events (eventHandlers) where

import Lambdananas.LanguageServer.Events.Hover
import Lambdananas.LanguageServer.Events.Init
import Lambdananas.LanguageServer.Events.Open
import Lambdananas.LanguageServer.Events.Save (onSave)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Server (Handlers)

eventHandlers :: Handlers LSM
eventHandlers = mconcat [onHover, onInit, onOpen, onSave]
