module Lambdananas.LanguageServer.State (State, removeDiagnsotics, moveDiagnostics) where

import Data.Maybe (fromMaybe)
import Lambdananas.Wrapper.Warn
import Language.LSP.Protocol.Types

-- | Note: We expect 'FilePath' to be absolute
type State = [(FilePath, [CodingStyleWarning])]

removeDiagnsotics :: Uri -> State -> State
removeDiagnsotics uri state = case uriToFilePath uri of
    Nothing -> state
    Just fp -> filter (\(key, _) -> key /= fp) state

moveDiagnostics :: Uri -> Uri -> State -> State
moveDiagnostics fromUri toUri state = fromMaybe state $ do
    fromFilePath <- uriToFilePath fromUri
    toFilePath <- uriToFilePath toUri
    return $ replace fromFilePath toFilePath state
  where
    replace from to (a : b)
        | fst a == from = (to, snd a) : b
        | otherwise = a : replace from to b
    replace _ _ [] = []
