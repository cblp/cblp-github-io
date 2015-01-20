{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.Monoid  ( (<>) )
import Hakyll       ( Context
                    , FeedConfiguration(..)
                    , applyAsTemplate
                    , constField
                    , dateField
                    , defaultContext
                    , getResourceBody
                    , hakyll
                    , listField
                    , loadAll
                    , loadAndApplyTemplate
                    , makeItem
                    , pandocCompiler
                    , recentFirst
                    , relativizeUrls
                    , renderRss
                    )

import Local.Hakyll ( cacheTemplates
                    , compileFiles
                    , compileFilesHtml
                    , compressCss
                    , copyFiles
                    , createFile
                    )


main :: IO ()
main = hakyll $ do
    copyFiles "images/*"
    compressCss "css/*"

    compileFilesHtml "posts/*" $
        pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    createFile "archive.html" $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
                listField "posts" postCtx (return posts) <>
                constField "title" "Archive"             <>
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

    createFile "feed.xml" $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let feedCtx         = constField "description" "" <>
                              defaultContext
            feedAuthorName  = "Yuriy Syrovetskiy"
            feedAuthorEmail = "cblp@cblp.su"
            feedTitle       = feedAuthorName
            feedDescription = feedAuthorName ++ "'s blog"
            feedRoot        = "http://cblp.github.io"
        renderRss FeedConfiguration{..} feedCtx posts

    compileFiles "index.html" $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
                listField "posts" postCtx (return posts) <>
                constField "title" "Recent posts"        <>
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    -- Static
    --match "contact.md" $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --        >>= relativizeUrls

    cacheTemplates "templates/*"


postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" <>
    defaultContext
