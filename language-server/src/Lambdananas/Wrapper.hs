{-# LANGUAGE ScopedTypeVariables #-}

module Lambdananas.Wrapper (
    getCodingStyleWarnings,
    LambdananasError (..),
) where

import Control.Exception
import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (dropWhileEnd, groupBy)
import Lambdananas.Wrapper.Warn (CodingStyleWarning (fileName), parseCodingStyleWarning)
import System.Exit (ExitCode (..))
import System.Process
import Text.Megaparsec as M

data LambdananasError
    = -- | The lambdananas binary could not be found
      CommandNotFound
    | -- | An error occured while parsing the output of Lambdananas.
      --
      -- The `String` is a human-readable message explaining the error
      OutputParsingError String
    | -- An unknown error occured. The `String` is a raw error message, for debugging only.
      UnknownError String
    deriving (Show)

-- | Invokes lambdananas and returns the reported 'CodingStyleWarning'
--
-- The 'FilePath' can be a directory or a file
--
-- The 'CodingStyleWarning' are grouped by source file
getCodingStyleWarnings ::
    FilePath ->
    IO (Either LambdananasError [(FilePath, [CodingStyleWarning])])
getCodingStyleWarnings filePath = runExceptT $ do
    rawOutput <- ExceptT $ runLambdananas filePath
    let trim = dropWhile isSpace . dropWhileEnd isSpace
        splitOutput = filter (not . null) $ trim <$> lines rawOutput
        parseWarning s = case M.parse parseCodingStyleWarning "" s of
            Left e -> Left $ OutputParsingError $ show e
            Right warn -> Right warn
        parseRes = parseWarning <$> splitOutput
        groupByFilePath l =
            (\group -> (fileName $ head group, group))
                <$> groupBy
                    (\w1 w2 -> fileName w1 == fileName w2)
                    l
    warns <- liftEither $ sequence parseRes
    return $ groupByFilePath warns

-- | Runs Lambdananas, passing it the provided `FilePath`.
--
-- On success, returns the raw stdout
runLambdananas :: FilePath -> IO (Either LambdananasError String)
runLambdananas fileToInspect = runExceptT $ do
    let process = proc "lambdananas-exe" [fileToInspect]
    (exitCode, stdout, stderr) <-
        ExceptT $
            catch
                (Right <$> readCreateProcessWithExitCode process "")
                (\(_ :: IOException) -> return $ Left CommandNotFound)
    liftEither $ case exitCode of
        ExitSuccess -> Right stdout
        -- TODO Check if portable
        ExitFailure 127 -> Left CommandNotFound
        ExitFailure _ -> Left $ UnknownError stderr
