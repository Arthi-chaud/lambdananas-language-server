{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.Reader
import Data.Maybe
import Data.Text as T
import Lambdananas.Wrapper
import Lambdananas.Wrapper.Warn (CodingStyleWarning)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

type State = [(FilePath, [CodingStyleWarning])]

type LSM = LspT () (ReaderT (MVar State) IO)

main :: IO ()
main = do
    state <- newMVar ([] :: State)
    let handlers = mconcat []
        config =
            ServerDefinition
                { parseConfig = const $ const $ Right ()
                , onConfigChange = const $ pure ()
                , defaultConfig = ()
                , configSection = "lambdananas"
                , doInitialize = \env _req -> do
                    runReaderT (runLspT env initialize) state
                    return (Right env)
                , staticHandlers = const handlers
                , interpretHandler = \env ->
                    Iso (\lsm -> runReaderT (runLspT env lsm) state) liftIO
                , options = defaultOptions
                }
    void $ runServer config

-- | Runs lambdananas, collects the warnings and store them in the mvar
initialize :: LSM ()
initialize = withIndefiniteProgress
    "Analysing Coding Style"
    NotCancellable
    $ do
        workspaceFolders <- fromMaybe [] <$> getWorkspaceFolders
        let folderToPath (WorkspaceFolder uri _) = uriToFilePath uri
            folders = maybeToList . folderToPath =<< workspaceFolders
        scanRes <- liftIO $ mapM getCodingStyleWarnings folders
        case sequence scanRes of
            Right state -> LspT $ ReaderT $ \_ -> do
                mstate <- ask
                liftIO $ putMVar mstate $ Prelude.concat state
            Left err ->
                sendNotification
                    SMethod_WindowShowMessage
                    ( ShowMessageParams MessageType_Error $
                        T.pack $
                            "Something went wrong!\n" ++ show err
                    )
