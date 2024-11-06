module Lambdananas.LanguageServer.Events.Hover (onHover) where

import Lambdananas.LanguageServer.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

onHover :: Handlers LSM
onHover = requestHandler SMethod_TextDocumentHover $ \_req responder -> do
    responder (Right (InR Null))
