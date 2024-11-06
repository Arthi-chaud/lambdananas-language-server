module Lambdananas.LanguageServer.State (State) where

import Lambdananas.Wrapper.Warn

type State = [(FilePath, [CodingStyleWarning])]
