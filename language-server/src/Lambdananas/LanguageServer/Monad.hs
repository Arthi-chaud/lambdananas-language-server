module Lambdananas.LanguageServer.Monad (LSM) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Lambdananas.LanguageServer.State
import Language.LSP.Server

type LSM = LspT () (ReaderT (MVar State) IO)
