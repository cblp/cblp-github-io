{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.Monoid  ( (<>) )
import Hakyll       ( Context
                    , FeedConfiguration(..)
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
                    , loadAll
                    , loadAllSnapshots
                    , loadAndApplyTemplate
                    , makeItem
                    , pandocCompiler
                    , saveSnapshot
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
    let loadPosts        = recentFirst =<< loadAll          "posts/*"
        loadPostsContent = recentFirst =<< loadAllSnapshots "posts/*" "content"

    copyFiles "images/*"
    compressCss "css/*"

    compileFilesHtml "posts/*" $
        pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    createFile "archive.html" $ do
        posts <- loadPosts
        let archiveCtx =
                listField "posts" postCtx (return posts) <>
                constField "title" "Archive"             <>
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

    createFile "feed.xml" $ do
        posts <- loadPostsContent
        let feedCtx         = descriptionAutoField <>
                              defaultContext
            feedAuthorName  = "Yuriy Syrovetskiy"
            feedAuthorEmail = "cblp@cblp.su"
            feedTitle       = feedAuthorName
            feedDescription = feedAuthorName ++ "'s blog"
            feedRoot        = "http://cblp.github.io"
        renderRss FeedConfiguration{..} feedCtx posts

    compileFiles "index.html" $ do
        posts <- loadPosts
        let indexCtx =
                listField "posts" postCtx (return posts) <>
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


descriptionAutoField :: Context String
descriptionAutoField = field "description" $ \item -> do
    mdescription <- getMetadataField (itemIdentifier item) "description"
    return $ case mdescription of
        Just description    -> description
        Nothing             -> itemBody item
