{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid  ( (<>) )
import Hakyll       ( Context
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
                    )

import Local.Hakyll ( cacheTemplates
                    , compileFiles
                    , compressCss
                    , copyFiles
                    , createFile
                    )


main :: IO ()
main = hakyll $ do
    copyFiles       "images/*"
    compressCss     "css/*"

    compileFiles    "posts/*" $
        pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    createFile      "archive.html" $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
                listField "posts" postCtx (return posts) <>
                constField "title" "Archive"             <>
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

    compileFiles    "index.html" $ do
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

    cacheTemplates  "templates/*"


postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" <>
    defaultContext
