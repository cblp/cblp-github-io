#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (forM)
import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.ByteString as ByteString (readFile)
import Data.ByteString.Char8 as ByteString (lines, pack, unlines)
import Data.List (elemIndices)
import Data.Text (Text)
import Data.Text as Text (pack)
import Data.Yaml as Yaml (decode)
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
                postFileContent <- liftIO $ ByteString.readFile postFile
                let postFileLines = ByteString.lines postFileContent
                    fmStart:fmEnd:_ = elemIndices documentStart postFileLines
                    Just (frontmatter :: Frontmatter) =
                        substr (fmStart + 1) fmEnd postFileLines
                        |> ByteString.unlines
                        |> Yaml.decode
                liftIO $ print frontmatter
                return (Text.pack "tag", postFile)
        liftIO $ print tags_paths

        -- TODO: write tag list
        -- TODO: write every tag file

  where
    documentStart = ByteString.pack "---"
    substr start end = drop start . take end


type Tag = Text


(|>) :: a -> (a -> b) -> b
x |> f = f x


data Frontmatter = Frontmatter
    { fm_tags :: [Tag]
    }
        deriving Show
deriveFromJSON defaultOptions{fieldLabelModifier = drop 3} ''Frontmatter
