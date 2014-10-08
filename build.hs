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
        tags_paths :: [(Tag, FilePath)] <- do
            forM postFiles $ \postFile -> do
                postFileLines <- readFileLines postFile
                let frontmatterStart:frontmatterEnd:_ =
                        elemIndices "---" postFileLines
                    frontmatter =
                        substr (frontmatterStart + 1) frontmatterEnd
                            postFileLines
                        |> unlines
                liftIO $ print frontmatter
                return ("tag", postFile)
        liftIO $ print tags_paths

        -- TODO: write tag list
        -- TODO: write every tag file

  where
    substr start end = drop start . take end


type Tag = String


(|>) :: a -> (a -> b) -> b
x |> f = f x
