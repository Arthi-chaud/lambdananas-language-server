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
import Language.LSP.Server

runServer :: IO ()
runServer = do
    state <- newMVar ([] :: State)
    let config =
            ServerDefinition
                { parseConfig = const $ const $ Right ()
                , onConfigChange = const $ pure ()
                , defaultConfig = ()
                , configSection = "lambdananas"
                , doInitialize = \env _req -> do
                    runReaderT (runLspT env initialize) state
                    return (Right env)
                , staticHandlers = const eventHandlers
                , interpretHandler = \env ->
                    Iso (\lsm -> runReaderT (runLspT env lsm) state) liftIO
                , options = defaultOptions
                }
    void $ Language.LSP.Server.runServer config
