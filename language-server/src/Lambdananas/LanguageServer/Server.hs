{-# LANGUAGE OverloadedStrings #-}

module Lambdananas.LanguageServer.Server (
    Lambdananas.LanguageServer.Server.runServer,
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Lambdananas.LanguageServer.Events
import Lambdananas.LanguageServer.Initialization
import Lambdananas.LanguageServer.State
import Language.LSP.Protocol.Types
import Language.LSP.Server

runServer :: IO ()
runServer = do
    state <- newMVar ([] :: State)
    let configOptions =
            defaultOptions
                { optCodeActionKinds = Just [CodeActionKind_QuickFix]
                , optTextDocumentSync =
                    Just $
                        -- See https://learn.microsoft.com/en-us/dotnet/api/microsoft.visualstudio.languageserver.protocol.textdocumentsyncoptions?view=visualstudiosdk-2022
                        TextDocumentSyncOptions
                            (Just True)
                            (Just TextDocumentSyncKind_Incremental)
                            (Just False) -- will save?
                            (Just False) -- will save?
                            (Just $ InR $ SaveOptions $ Just False) -- get file content on save
                }
    let config =
            ServerDefinition
                { parseConfig = const $ const $ Right ()
                , onConfigChange = const $ pure ()
                , defaultConfig = ()
                , configSection = "lambdananas"
                , doInitialize = \env _req -> do
                    runReaderT (runLspT env initialize) state
                    -- TODO In case of error, return ResponseError
                    return (Right env)
                , staticHandlers = const eventHandlers
                , interpretHandler = \env ->
                    Iso (\lsm -> runReaderT (runLspT env lsm) state) liftIO
                , options = configOptions
                }
    void $ Language.LSP.Server.runServer config
