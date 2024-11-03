module Lambdananas.Wrapper () where

import System.Exit (ExitCode (..))
import System.Process

data LambdananasError
    = -- | The lambdananas binary could not be found
      CommandNotFound
    | -- | An error occured while parsing the output of Lambdananas.
      --
      -- The `String` is a human-readable message explaining the error
      OutputParsingError String
    | -- An unknown error occured. The `String` is a raw error message, for debugging only.
      UnknownError String

-- | Describes the severity of a coding style mistake
data SeverityLevel = Major | Minor | Info

data CodingStyleMistake = CodingStyleMistake
    { level :: SeverityLevel
    , code :: String
    , fileName :: String
    , description :: String
    }

-- | Runs Lambdananas, passing it the provided `FilePath`.
--
-- On success, returns the raw stdout
runLambdananas :: FilePath -> IO (Either LambdananasError String)
runLambdananas fileToInspect = do
    let process = proc "lambdananas" [fileToInspect]
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    case exitCode of
        ExitSuccess -> return $ Right stdout
        -- TODO Check if portable
        ExitFailure 127 -> return (Left CommandNotFound)
        ExitFailure _ -> return (Left $ UnknownError stderr)
