module Lambdananas.LanguageServer.Error (sendErrorMessage) where

import qualified Data.Text as T
import Lambdananas.LanguageServer.Monad (LSM)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

sendErrorMessage :: String -> LSM ()
sendErrorMessage msg = do
    sendNotification
        SMethod_WindowShowMessage
        ( ShowMessageParams MessageType_Error $
            T.pack $
                "Something went wrong!\n" ++ msg
        )
