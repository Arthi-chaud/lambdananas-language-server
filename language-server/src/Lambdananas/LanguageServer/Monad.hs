{-# LANGUAGE BangPatterns #-}

module Lambdananas.LanguageServer.Monad (
    LSM,
    getState,
    setState,
    withState,
    getCodingStyleWarningForFile,
) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Maybe
import Lambdananas.LanguageServer.State
import Lambdananas.Wrapper.Warn
import Language.LSP.Protocol.Types
import Language.LSP.Server

-- | The monad used across the LSP
-- to allow accessing the state and run IO
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

withState :: (State -> (a, State)) -> LSM a
withState f = do
    state <- getState
    let !(res, newState) = f state
    setState newState
    return res
