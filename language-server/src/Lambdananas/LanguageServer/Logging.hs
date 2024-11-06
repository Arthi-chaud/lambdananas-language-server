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

debugLog :: String -> LSM ()
debugLog = sendLog Debug

infoLog :: String -> LSM ()
infoLog = sendLog Info

warnLog :: String -> LSM ()
warnLog = sendLog Warning

errorLog :: String -> LSM ()
errorLog = sendLog Error

sendLog :: Severity -> String -> LSM ()
sendLog severity msg = logToLogMessage <& (T.pack (severityLabel severity ++ msg) `WithSeverity` Debug)
  where
    severityLabel Debug = "[DEBUG] "
    severityLabel Info = "[INFO] "
    severityLabel Warning = "[WARN] "
    severityLabel Error = "[ERROR] "
