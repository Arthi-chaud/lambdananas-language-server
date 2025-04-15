{-# LANGUAGE ScopedTypeVariables #-}

module Lambdananas.Wrapper (
    getCodingStyleWarnings,
    lambdananasExists,
    LambdananasError (..),
) where

import Control.Exception
import Control.Monad.Except
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List (dropWhileEnd, groupBy, isInfixOf)
import Lambdananas.Wrapper.Warn (CodingStyleWarning (fileName), parseCodingStyleWarning)
import System.Exit (ExitCode (..))
import System.Process
import Text.Megaparsec as M

data LambdananasError
    = -- | The lambdananas binary could not be found
      CommandNotFound
    | SourceFileNotFound
    | -- | An error occured while parsing the output of Lambdananas.
      --
      -- The `String` is a human-readable message explaining the error
      OutputParsingError String
    | -- An unknown error occured. The `String` is a raw error message, for debugging only.
      UnknownError String

-- | Human-readable error messages that can be sent to the client
instance Show LambdananasError where
    show CommandNotFound = "The lambdananas executable was not found. Is 'lambdananas-exe' or 'lambdananas' in your PATH ?"
    show SourceFileNotFound = "The file was not found by lambdananas."
    show (OutputParsingError str) =
        "lambdananas gave us an unexpected output:\n" ++ str
    show (UnknownError str) = "An unknown error occured:\n" ++ str

-- | Invokes lambdananas and returns the reported 'CodingStyleWarning's
--
-- The 'FilePath' can be a directory or a file
--
-- The 'CodingStyleWarning' are grouped by source file
getCodingStyleWarnings ::
    FilePath ->
    ExceptT LambdananasError IO [(FilePath, [CodingStyleWarning])]
getCodingStyleWarnings filePath = do
    rawOutput <- runLambdananas [filePath]
    let trim = dropWhile isSpace . dropWhileEnd isSpace
        splitOutput =
            filter
                (not . null)
                (trim <$> lines rawOutput)
        parseWarning s = case M.parse (parseCodingStyleWarning filePath) "" s of
            Left e -> Left $ OutputParsingError $ show e
            Right warn -> Right warn
        groupByFilePath l =
            (\group -> (fileName $ head group, group))
                <$> groupBy
                    (\w1 w2 -> fileName w1 == fileName w2)
                    l
    warns <- liftEither $ parseWarning `mapM` splitOutput
    return $ groupByFilePath warns

-- | Checks lambdananas is in PATH
lambdananasExists :: IO Bool
lambdananasExists = do
    res <- runExceptT $ runLambdananas ["-h"]
    return $ isRight res

-- | Runs Lambdananas, passing it the provided arguments
--
-- On success, returns the raw stdout
runLambdananas :: [String] -> ExceptT LambdananasError IO String
runLambdananas args =
    catchError
        (go "lambdananas-exe") -- The binary name when installed with stack
        (\(_ :: LambdananasError) -> go "lambdananas") -- Alternative name
  where
    go binName = do
        let process = proc binName args
        (exitCode, stdout, stderr) <-
            ExceptT $
                catch
                    (Right <$> readCreateProcessWithExitCode process "")
                    (\(_ :: IOException) -> return $ Left CommandNotFound)
        case exitCode of
            ExitSuccess -> return stdout
            -- TODO Check if portable
            ExitFailure 127 -> throwError CommandNotFound
            ExitFailure _
                | "openFile: does not exist" `isInfixOf` stderr ->
                    throwError SourceFileNotFound
                | otherwise -> throwError $ UnknownError stderr
