module Lambdananas.LanguageServer.Monad (LSM, getState, setState) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Lambdananas.LanguageServer.State
import Language.LSP.Server

type LSM = LspT () (ReaderT (MVar State) IO)

getState :: LSM State
getState = lift ask >>= liftIO . readMVar

setState :: State -> LSM ()
setState state = do
    mvar <- lift ask
    _ <- liftIO (swapMVar mvar state)
    return ()
