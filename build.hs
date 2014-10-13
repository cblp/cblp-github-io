#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString (putStr, readFile)
import Data.ByteString.Char8 as ByteString (lines, pack, unlines)
import Data.List (elemIndices)
import Data.Map (Map)
import Data.Map as Map (empty, insertWith)
import Data.Text (Text)
import Data.Yaml as Yaml (decode, encode)
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
        postFilesContents <- liftIO $ mapM ByteString.readFile postFiles
        let tagPaths = mapFromListCollectingValues
                [ (tag, postFile)
                | (tags, postFile) <- zip (map readTags postFilesContents)
                                          postFiles
                , tag <- tags
                ]

        liftIO $ ByteString.putStr $ Yaml.encode tagPaths

        -- TODO: write tag list
        -- TODO: write every tag file


type Tag = Text -- TODO: String?


(|>) :: a -> (a -> b) -> b
x |> f = f x


data Frontmatter = Frontmatter
    { fm_tags :: [Tag]
    }
        deriving Show


readTags :: ByteString -> [Tag]
readTags = fm_tags . readFrontmatter


readFrontmatter :: ByteString -> Frontmatter
readFrontmatter fileContents =
    let fileLines = ByteString.lines fileContents
        fmStart:fmEnd:_ = elemIndices documentStart fileLines
        Just frontmatter = sublist (fmStart + 1) fmEnd fileLines
                           |> ByteString.unlines
                           |> Yaml.decode
    in  frontmatter

  where
    documentStart = ByteString.pack "---"
    sublist start end = drop start . take end


mapFromListCollectingValues :: Ord k => [(k, v)] -> Map k [v]
mapFromListCollectingValues =
    foldr (\(k, v) -> insertWith (++) k [v]) empty


-- TH should be at the end of file
deriveFromJSON defaultOptions{fieldLabelModifier = drop 3} ''Frontmatter
