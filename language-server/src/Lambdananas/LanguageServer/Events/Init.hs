module Lambdananas.LanguageServer.Events.Init (onInit) where

import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Server

-- | Callback on init (after initialize?)
onInit :: Handlers LSM
onInit = notificationHandler SMethod_Initialized $ \_ -> do
    debugLog "Init event received"
    return ()
