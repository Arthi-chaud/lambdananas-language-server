{-# LANGUAGE RecordWildCards #-}

module Lambdananas.Wrapper.Warn (
    SeverityLevel (..),
    CodingStyleWarning (..),
    parseCodingStyleWarning,
) where

import Data.List (isPrefixOf)
import Data.Void
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, string, string', upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Describes the severity of a coding style mistake
data SeverityLevel = Major | Minor | Info deriving (Show, Eq)

parseSeverityLevel :: Parser SeverityLevel
parseSeverityLevel =
    choice
        [ Major <$ string' "Major"
        , Minor <$ string' "Minor"
        , Info <$ string' "Info"
        ]

-- | A coding style mistake reported by lambdananas
--
-- (I don't like the name 'warning', but it's to mirror the data type from lambdananas)
data CodingStyleWarning = CodingStyleWarning
    { level :: SeverityLevel
    , ruleCode :: String
    , fileName :: String
    , -- One-indexed
      line :: Int
    , description :: String
    }
    deriving (Show, Eq)

parseRuleCode :: Parser String
parseRuleCode = do
    haskellCSLabel <- upperChar
    _ <- char '-'
    shortCode <- many alphaNumChar
    return $ haskellCSLabel : '-' : shortCode

-- | Parses a coding style warning of the following form
--
-- @
-- ./app/Main.hs:73: MINOR:H-F3 # too long line
--
-- ./src/Lambdananas/Wrapper.hs contains forbidden extensions
--  (Bug)
--  contains forbidden extensions
-- @
parseCodingStyleWarning :: FilePath -> Parser CodingStyleWarning
parseCodingStyleWarning fp = try (parseBogusExtensionWarning fp) <|> try parseForbiddenExtensionWarning <|> parseRegularWarning

parseBogusExtensionWarning :: FilePath -> Parser CodingStyleWarning
parseBogusExtensionWarning filepath = do
    _ <- many (char ' ') *> string "contains forbidden extensions"
    return
        CodingStyleWarning
            { fileName = filepath
            , level = Major
            , line = 1
            , description = "Lambdananas could not parse this file because of a non-standard syntax"
            , ruleCode = "H-E1"
            }

parseRegularWarning :: Parser CodingStyleWarning
parseRegularWarning = do
    fileName <- manyTill asciiChar (char ':')
    line <- L.decimal
    level <- string ": " *> parseSeverityLevel
    ruleCode <- char ':' *> parseRuleCode
    rawDescription <- string " # " *> many asciiChar
    let description = replace rawDescription fileName (takeFileName fileName)
    return CodingStyleWarning{..}

parseForbiddenExtensionWarning :: Parser CodingStyleWarning
parseForbiddenExtensionWarning = do
    let forbiddenExtensionStr = " contains forbidden extensions"
    fileName <- manyTill asciiChar (string forbiddenExtensionStr)
    return
        CodingStyleWarning
            { fileName = fileName
            , line = 1
            , level = Major
            , description = "language extensions are forbidden"
            , ruleCode = "H-E1"
            }

replace :: String -> String -> String -> String
replace haystack [] _ = haystack
replace [] _ _ = []
replace haystack@(c : cs) needle replacement
    | needle `isPrefixOf` haystack = replacement ++ drop (length needle) haystack
    | otherwise = c : replace cs needle replacement
