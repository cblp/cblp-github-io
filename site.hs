{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.Monoid  ( (<>) )
import Hakyll       ( Compiler
                    , Context
                    , FeedConfiguration(..)
                    , Item
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
    let loadPostsContent = loadAllSnapshots "posts/*" "content" >>= recentFirst
        loadPostsWidgets = loadAllSnapshots "posts/*" "widget"  >>= recentFirst

    copyFiles "images/*"
    compressCss "css/*"

    compileFilesHtml "posts/*" $
        pandocCompiler
            >>= saveSnapshot "content"
            >>= applyTemplate_postWidget                            postCtx
            >>= saveSnapshot "widget"
            >>= applyTemplate_postPage                              postCtx

    createFile "archive.html" $ do
        posts <- loadPostsWidgets
        let archiveCtx =
                listField "posts" postCtx (return posts) <>
                constField "title" "Archive"             <>
                defaultContext

        makeItem ""
            >>= applyTemplate_archive                         archiveCtx
            >>= applyTemplate_page                            archiveCtx

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
        posts <- loadPostsWidgets
        let indexCtx =
                listField "posts" postCtx (return posts) <>
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= applyTemplate_page  indexCtx

    compileFiles "cv.html" $ do
        getResourceBody
            >>= applyTemplate_default defaultContext

    cacheTemplates "templates/*"


postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d"
       <> descriptionAutoField
       <> defaultContext


descriptionAutoField :: Context String
descriptionAutoField = field "description" $ \item -> do
    mdescription <- getMetadataField (itemIdentifier item) "description"
    return $ case mdescription of
        Just description    -> description
        Nothing             -> itemBody item


applyTemplate_default :: Context a -> Item a -> Compiler (Item String)
applyTemplate_default ctx item =
    loadAndApplyTemplate "templates/default.html" ctx item
    >>= relativizeUrls


applyTemplate_page :: Context String -> Item String -> Compiler (Item String)
applyTemplate_page ctx item =
    loadAndApplyTemplate "templates/page.html" ctx item
    >>= applyTemplate_default ctx


applyTemplate_postPage :: Context String -> Item String -> Compiler (Item String)
applyTemplate_postPage ctx item =
    loadAndApplyTemplate "templates/postPage.html" ctx item
    >>= applyTemplate_page ctx


applyTemplate_archive :: Context String -> Item String -> Compiler (Item String)
applyTemplate_archive ctx item =
    loadAndApplyTemplate "templates/archive.html" ctx item
    >>= applyTemplate_page ctx


applyTemplate_postWidget :: Context String -> Item String -> Compiler (Item String)
applyTemplate_postWidget = loadAndApplyTemplate "templates/postWidget.html"
