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

-- | Invokes lambdananas and returns the reported 'CodingStyleWarning'
--
-- The 'FilePath' can be a directory or a file
--
-- The 'CodingStyleWarning' are grouped by source file
getCodingStyleWarnings ::
    FilePath ->
    IO (Either LambdananasError [(FilePath, [CodingStyleWarning])])
getCodingStyleWarnings filePath = runExceptT $ do
    rawOutput <- ExceptT $ runLambdananas [filePath]
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
    warns <- liftEither $ mapM parseWarning splitOutput
    return $ groupByFilePath warns

-- | Checks lambdananas is in PATH
lambdananasExists :: IO Bool
lambdananasExists = isRight <$> runLambdananas ["-h"]

-- | Runs Lambdananas, passing it the provided arguments
--
-- On success, returns the raw stdout
runLambdananas :: [String] -> IO (Either LambdananasError String)
runLambdananas args =
    catch
        (go "lambdananas-exe") -- The binary name when installed with stack
        (\(_ :: IOException) -> go "lambdananas") -- Alternative name
  where
    go binName = runExceptT $ do
        let process = proc binName args
        (exitCode, stdout, stderr) <-
            ExceptT $
                catch
                    (Right <$> readCreateProcessWithExitCode process "")
                    (\(_ :: IOException) -> return $ Left CommandNotFound)
        liftEither $ case exitCode of
            ExitSuccess -> Right stdout
            -- TODO Check if portable
            ExitFailure 127 -> Left CommandNotFound
            ExitFailure _
                | "openFile: does not exist" `isInfixOf` stderr ->
                    Left SourceFileNotFound
                | otherwise -> Left $ UnknownError stderr
