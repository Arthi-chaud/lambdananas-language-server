{-# LANGUAGE OverloadedStrings #-}

module Lambdananas.LanguageServer.Initialization (initialize) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
import Lambdananas.LanguageServer.Diagnostic
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
        workspaceFolders <- fromMaybe [] <$> getWorkspaceFolders
        let folderToPath (WorkspaceFolder uri _) = uriToFilePath uri
            folders = mapMaybe folderToPath workspaceFolders
        scanRes <- liftIO $ mapM getCodingStyleWarnings folders
        case Prelude.concat <$> sequence scanRes of
            Right state -> do
                mstate <- lift ask
                liftIO $ putMVar mstate state
                forM_ (fst <$> state) $ \filePath ->
                    let normUri = toNormalizedUri $ filePathToUri filePath
                     in emitDiagnostics normUri
            Left err ->
                sendNotification
                    SMethod_WindowShowMessage
                    ( ShowMessageParams MessageType_Error $
                        T.pack $
                            "Something went wrong!\n" ++ show err
                    )
