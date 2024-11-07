module Lambdananas.LanguageServer.State (State) where

import Lambdananas.Wrapper.Warn

-- | Note: We expect 'FilePath' to be absolute
type State = [(FilePath, [CodingStyleWarning])]
