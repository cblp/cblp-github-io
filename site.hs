{-# LANGUAGE RecordWildCards #-}

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
import Templates    ( applyTemplate_archive
                    , applyTemplate_default
                    , applyTemplate_page
                    , applyTemplate_postPage
                    , applyTemplate_postWidget
                    )


main :: IO ()
main = hakyll $ do
    let loadPostsContent = loadAllSnapshots "posts/*" "content" >>= recentFirst
        loadPostsWidgets = loadAllSnapshots "posts/*" "widget"  >>= recentFirst

    copyFiles "images/*"
    compressCss "css/*"

    compileFilesHtml "posts/*" $
        pandocCompiler  >>= saveSnapshot "content"
                        >>= applyTemplate_postWidget    postCtx
                        >>= saveSnapshot "widget"
                        >>= applyTemplate_postPage      postCtx

    createFile "archive.html" $ do
        posts <- loadPostsWidgets
        let archiveCtx =   listField "posts" postCtx (return posts)
                        <> constField "title" "Archive"
                        <> defaultContext

        makeItem "" >>= applyTemplate_archive   archiveCtx
                    >>= applyTemplate_page      archiveCtx

    createFile "feed.xml" $ do
        posts <- loadPostsContent
        let feedCtx         =  descriptionAutoField
                            <> defaultContext
            feedAuthorName  = "Yuriy Syrovetskiy"
            feedAuthorEmail = "cblp@cblp.su"
            feedTitle       = feedAuthorName
            feedDescription = feedAuthorName ++ "'s blog"
            feedRoot        = "http://cblp.github.io"

        renderRss FeedConfiguration{..} feedCtx posts

    compileFiles "index.html" $ do
        posts <- loadPostsWidgets
        let indexCtx = listField "posts" postCtx (return posts)
                    <> defaultContext

        getResourceBody >>= applyAsTemplate     indexCtx
                        >>= applyTemplate_page  indexCtx

    compileFiles "cv.html" $
        getResourceBody >>= applyTemplate_default defaultContext

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
