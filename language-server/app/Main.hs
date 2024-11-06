{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lambdananas.LanguageServer.Server (runServer)

main :: IO ()
main = runServer
