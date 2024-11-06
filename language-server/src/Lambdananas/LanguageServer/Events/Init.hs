module Lambdananas.LanguageServer.Events.Init (onInit) where

import Lambdananas.LanguageServer.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Server

onInit :: Handlers LSM
onInit = notificationHandler SMethod_Initialized $ \_ -> do
    return ()

-- state <- liftIO . takeMVar =<< lift ask
-- forM_ (fst <$> state) $ \filePath ->
--     let nrmUri = toNormalizedUri $ filePathToUri filePath
--      in emitDiagnostics nrmUri
