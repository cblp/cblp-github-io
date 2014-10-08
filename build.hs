#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM)
import Data.List (elemIndices)
--import Data.Map (Map)
import Development.Shake


main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["_site"]

    "_site" ~> do
        need ["tags"]
        cmd "jekyll" "build"

    "tags" ~> do
        postFiles <- getDirectoryFiles "" ["_posts/*.md"]

        -- read tags
        tags :: [(Tag, FilePath)] <- do
            forM postFiles $ \postFile -> do
                postFileLines <- readFileLines postFile
                let frontmatterStart:frontmatterEnd:_ =
                        elemIndices "---" postFileLines
                liftIO $ print (frontmatterStart, frontmatterEnd)
                return ("tag", postFile)
        liftIO $ print tags

        -- TODO: write tag list
        -- TODO: write every tag file


type Tag = String
