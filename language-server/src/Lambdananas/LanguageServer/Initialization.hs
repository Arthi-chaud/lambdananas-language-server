{-# LANGUAGE OverloadedStrings #-}

module Lambdananas.LanguageServer.Initialization (initialize) where

import Control.Monad.Reader
import Lambdananas.LanguageServer.Logging (debugLog, errorLog)
import Lambdananas.LanguageServer.Monad
import Lambdananas.Wrapper
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

initialize :: LSM ()
initialize = withIndefiniteProgress
    "Analysing Coding Style"
    NotCancellable
    $ do
        exists <- liftIO lambdananasExists
        if exists
            then debugLog "Lambdanas was found!"
            else do
                errorLog "Lambdananas was not found"
                sendNotification
                    SMethod_WindowShowMessage
                    ( ShowMessageParams
                        MessageType_Error
                        "Lambdananas was not found in PATH. Did you install it?"
                    )
