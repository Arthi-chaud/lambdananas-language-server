{-# LANGUAGE RecordWildCards #-}

module Lambdananas.Wrapper.Warn (
    SeverityLevel (..),
    CodingStyleWarning (..),
    parseCodingStyleWarning,
) where

import Data.Void
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
-- @
parseCodingStyleWarning :: Parser CodingStyleWarning
parseCodingStyleWarning = try parseForbiddenExtensionWarning <|> parseRegularWarning

parseRegularWarning :: Parser CodingStyleWarning
parseRegularWarning = do
    fileName <- manyTill asciiChar (char ':')
    line <- L.decimal
    level <- string ": " *> parseSeverityLevel
    ruleCode <- char ':' *> parseRuleCode
    description <- string " # " *> many asciiChar
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
