module Templates where

import Hakyll   ( Compiler
                , Context
                , Item
                , loadAndApplyTemplate
                , relativizeUrls
                )


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
