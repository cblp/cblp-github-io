#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.ByteString as ByteString (readFile)
import Data.ByteString.Char8 as ByteString (lines, pack, unlines)
import Data.List (elemIndices)
import Data.Text (Text)
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
            concatM postFiles $ \postFile -> do
                tags <- liftIO $ readTags postFile
                return [(tag, postFile) | tag <- tags]

        liftIO $ print tags_paths

        -- TODO: write tag list
        -- TODO: write every tag file


type Tag = Text


(|>) :: a -> (a -> b) -> b
x |> f = f x


concatM :: (Monad m, Functor m) => [a] -> (a -> m [b]) -> m [b]
concatM xs f = concat <$> forM xs f


data Frontmatter = Frontmatter
    { fm_tags :: [Tag]
    }
        deriving Show


readTags :: FilePath -> IO [Tag]
readTags file = fm_tags <$> readFrontmatter file


readFrontmatter :: FilePath -> IO Frontmatter
readFrontmatter file = do
    contents <- ByteString.readFile file
    let fileLines = ByteString.lines contents
        fmStart:fmEnd:_ = elemIndices documentStart fileLines
        Just (frontmatter :: Frontmatter) =
            sublist (fmStart + 1) fmEnd fileLines
            |> ByteString.unlines
            |> Yaml.decode
    return frontmatter

  where
    documentStart = ByteString.pack "---"
    sublist start end = drop start . take end


-- TH should be at the end of file
deriveFromJSON defaultOptions{fieldLabelModifier = drop 3} ''Frontmatter
