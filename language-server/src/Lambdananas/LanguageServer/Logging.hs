{-# LANGUAGE FlexibleContexts #-}

module Lambdananas.LanguageServer.Logging (
    debugLog,
    infoLog,
    warnLog,
    errorLog,
    sendLog,
) where

import Colog.Core (Severity (..), WithSeverity (WithSeverity), (<&))
import qualified Data.Text as T
import Lambdananas.LanguageServer.Monad
import Language.LSP.Logging (logToLogMessage)

-- | Send debug log
debugLog :: String -> LSM ()
debugLog = sendLog Debug

-- | Send info log
infoLog :: String -> LSM ()
infoLog = sendLog Info

-- | Send warning log
warnLog :: String -> LSM ()
warnLog = sendLog Warning

-- | Send error log
errorLog :: String -> LSM ()
errorLog = sendLog Error

-- | Send log to the LSP Client
--
-- This log may not be visible to the user.
-- Use 'sendErrorMessage' if that's what you want instead
sendLog :: Severity -> String -> LSM ()
sendLog severity msg = logToLogMessage <& (T.pack (severityLabel severity ++ msg) `WithSeverity` Debug)
  where
    severityLabel Debug = "[DEBUG] "
    severityLabel Info = "[INFO] "
    severityLabel Warning = "[WARN] "
    severityLabel Error = "[ERROR] "
