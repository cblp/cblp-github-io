{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Data.Maybe   ( fromMaybe )
import Data.Monoid  ( (<>) )
import Hakyll       ( Context
                    , FeedConfiguration(..)
                    , Item(..)
                    , applyAsTemplate
                    , constField
                    , dateField
                    , defaultContext
                    , field
                    , getMetadataField
                    , getResourceBody
                    , hakyll
                    , itemBody
                    , itemIdentifier
                    , listField
                    , loadAllSnapshots
                    , makeItem
                    , pandocCompiler
                    , saveSnapshot
                    , recentFirst
                    , renderRss
                    )

import Local.Hakyll ( cacheTemplates
                    , compileFiles
                    , compileFilesHtml
                    , compressCss
                    , copyFiles
                    , createFile
                    )
import Local.Prelude
import Templates    ( Template(..)
                    , applyTemplate
                    )


main :: IO ()
main = hakyll $ do
    cacheTemplates      "templates/*"

    createFile          "archive.html"  archiveCompiler
    compressCss         "css/*"
    compileFiles        "cv.html"       cvCompiler
    copyFiles           "favicon.ico"
    createFile          "feed.xml"      feedCompiler
    compileFiles        "index.html"    indexCompiler
    copyFiles           "images/*"
    compileFilesHtml    "posts/*"       postsCompiler

  where

    postCtx = dateField "date" "%Y-%m-%d"
           <> descriptionAutoField
           <> defaultContext

    loadPostsContent = loadAllSnapshots "posts/*" "content" >>= recentFirst
    loadPostsWidgets = loadAllSnapshots "posts/*" "widget"  >>= recentFirst

    postsCompiler = pandocCompiler  >>= saveSnapshot "content"
                                    >>= applyTemplate PostWidget  postCtx
                                    >>= saveSnapshot "widget"
                                    >>= applyTemplate PostPage    postCtx

    archiveCompiler = do
        posts <- loadPostsWidgets
        let archiveCtx = listField "posts" postCtx (return posts)
                      <> constField "title" "Archive"
                      <> defaultContext

        makeItem "" >>= applyTemplate Archive archiveCtx
                    >>= applyTemplate Page    archiveCtx

    feedCompiler = do
        posts <- loadPostsContent
        let feedCtx         = descriptionAutoField
                          <>  defaultContext
            feedAuthorName  = "Yuriy Syrovetskiy"
            feedAuthorEmail = "cblp@cblp.su"
            feedTitle       = feedAuthorName
            feedDescription = feedAuthorName ++ "'s blog"
            feedRoot        = "http://cblp.github.io"

        renderRss FeedConfiguration{..} feedCtx posts

    indexCompiler = do
        posts <- loadPostsWidgets
        let indexCtx = listField "posts" postCtx (return posts)
                    <> defaultContext

        getResourceBody >>= applyAsTemplate       indexCtx
                        >>= applyTemplate Content indexCtx
                        >>= applyTemplate Page    indexCtx

    cvCompiler = getResourceBody  >>= applyTemplate Content defaultContext
                                  >>= applyTemplate Default defaultContext


descriptionAutoField :: Context String
descriptionAutoField = field "description" $
    \Item{itemIdentifier, itemBody} ->
        getMetadataField itemIdentifier "description"
        & fmap (fromMaybe itemBody)
