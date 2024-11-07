{-# LANGUAGE OverloadedStrings #-}

module Lambdananas.LanguageServer.Events.Hover (onHover) where

import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

onHover :: Handlers LSM
onHover = requestHandler SMethod_TextDocumentHover $ \_req responder -> do
    debugLog "Hover event received"
    responder (Right (InR Null))
