module Lambdananas.LanguageServer.Monad (
    LSM,
    getState,
    setState,
    getCodingStyleWarningForFile,
) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Maybe
import Lambdananas.LanguageServer.State
import Lambdananas.Wrapper.Warn
import Language.LSP.Protocol.Types
import Language.LSP.Server

type LSM = LspT () (ReaderT (MVar State) IO)

getState :: LSM State
getState = lift ask >>= liftIO . readMVar

setState :: State -> LSM ()
setState state = do
    mvar <- lift ask
    _ <- liftIO (swapMVar mvar state)
    return ()

getCodingStyleWarningForFile :: Uri -> LSM [CodingStyleWarning]
getCodingStyleWarningForFile uri = do
    state <- getState
    case uriToFilePath uri of
        Nothing -> return []
        Just filePath -> return $ fromMaybe [] $ lookup filePath state
