#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<$>))
import Control.Arrow ((>>>))
import Control.Monad (forM)
import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.ByteString as ByteString (readFile)
import Data.ByteString.Char8 as ByteString (lines, pack, unlines)
import Data.List (elemIndices)
import Data.Map (Map)
import Data.Map as Map (empty, insertWith)
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
        tagPaths :: Map Tag [FilePath] <-
            let readTagsWithFilenames postFile = do
                    tags <- liftIO $ readTags postFile
                    return [(tag, postFile) | tag <- tags]
            in  forM postFiles readTagsWithFilenames
                $> concat
                $> mapFromListCollectingValues

        liftIO $ print tagPaths

        -- TODO: write tag list
        -- TODO: write every tag file


type Tag = Text -- TODO: String?


(|>) :: a -> (a -> b) -> b
x |> f = f x


($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap
infixl 4 $>


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


mapFromListCollectingValues :: Ord k => [(k, v)] -> Map k [v]
mapFromListCollectingValues =
    foldr (\(k, v) -> insertWith (++) k [v]) empty


-- TH should be at the end of file
deriveFromJSON defaultOptions{fieldLabelModifier = drop 3} ''Frontmatter
