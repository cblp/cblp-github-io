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
import Templates    ( Template(..)
                    , applyTemplate
                    )


main :: IO ()
main = hakyll $ do
    let loadPostsContent = loadAllSnapshots "posts/*" "content" >>= recentFirst
        loadPostsWidgets = loadAllSnapshots "posts/*" "widget"  >>= recentFirst

    copyFiles "images/*"
    compressCss "css/*"

    compileFilesHtml "posts/*" $
        pandocCompiler  >>= saveSnapshot "content"
                        >>= applyTemplate PostWidget    postCtx
                        >>= saveSnapshot "widget"
                        >>= applyTemplate PostPage      postCtx

    createFile "archive.html" $ do
        posts <- loadPostsWidgets
        let archiveCtx =   listField "posts" postCtx (return posts)
                        <> constField "title" "Archive"
                        <> defaultContext

        makeItem "" >>= applyTemplate Archive   archiveCtx
                    >>= applyTemplate Page      archiveCtx

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
                        >>= applyTemplate Page  indexCtx

    compileFiles "cv.html" $
        getResourceBody >>= applyTemplate Default defaultContext

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
