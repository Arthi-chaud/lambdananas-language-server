module Lambdananas.LanguageServer.State (State, removeDiagnsotics, moveDiagnostics) where

import Data.Maybe (fromMaybe)
import Lambdananas.Wrapper.Warn
import Language.LSP.Protocol.Types

-- | The State shared across the LSP
--
-- Note: We expect 'FilePath' to be absolute
type State = [(FilePath, [CodingStyleWarning])]

-- | Remove diagnostics for a given file in the state
removeDiagnsotics :: Uri -> State -> State
removeDiagnsotics uri state = case uriToFilePath uri of
    Nothing -> state
    Just fp -> filter (\(key, _) -> key /= fp) state

-- | Move diagnostics from a file to another
--
-- To use when a file is renamed
moveDiagnostics :: Uri -> Uri -> State -> State
moveDiagnostics fromUri toUri state = fromMaybe state $ do
    -- If either fail, just return the original state
    fromFilePath <- uriToFilePath fromUri
    toFilePath <- uriToFilePath toUri
    return $ replace fromFilePath toFilePath state
  where
    replace from to (a@(fp, diagns) : as)
        | fp == from = (to, diagns) : as
        | otherwise = a : replace from to as
    replace _ _ [] = []
