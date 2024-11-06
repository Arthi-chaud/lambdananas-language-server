module Lambdananas.LanguageServer.Events (eventHandlers) where

import Lambdananas.LanguageServer.Events.Hover (onHover)
import Lambdananas.LanguageServer.Events.Init (onInit)
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Server (Handlers)

eventHandlers :: Handlers LSM
eventHandlers = mconcat [onHover, onInit]
