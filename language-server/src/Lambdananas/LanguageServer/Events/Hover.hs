{-# LANGUAGE OverloadedStrings #-}

module Lambdananas.LanguageServer.Events.Hover (onHover) where

import Lambdananas.LanguageServer.Logging (debugLog)
import Lambdananas.LanguageServer.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

onHover :: Handlers LSM
onHover = requestHandler SMethod_TextDocumentHover $ \req responder -> do
    debugLog "Hover event received"
    responder $ Right (InR Null)

-- let fileUri = req ^. params . textDocument . uri
--     hoverLine = fromIntegral $ req ^. params . position . line
--     emptyResponse = Right (InR Null)
-- state <- getState
-- let response = fromMaybe emptyResponse $ do
--         filePath <- uriToFilePath fileUri
--         warns <- lookup filePath state
--         let matchingWarns = filter (\w -> (Warn.line w - 1) == hoverLine) warns
--         guard (not $ null matchingWarns)
--         let formattedWarns =
--                 ( \w ->
--                     printf
--                         "%s (**%s** - %s)"
--                         (Warn.description w)
--                         (show $ Warn.level w)
--                         (Warn.ruleCode w)
--                 )
--                     <$> matchingWarns
--             linkString = printf "Go to the [Coding Style Reference](%s)" codingStyleUrl
--             markupContent =
--                 MarkupContent
--                     MarkupKind_Markdown
--                     (T.pack $ unlines $ formattedWarns ++ ["---", linkString])
--         return $ Right (InL (Hover (InL markupContent) Nothing))
-- responder response
