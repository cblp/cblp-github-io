module Templates    ( Template(..)
                    , applyTemplate
                    )
where

import Hakyll   ( Compiler
                , Context
                , Identifier
                , Item
                , loadAndApplyTemplate
                , relativizeUrls
                )


type HakyllTemplater = Context String -> Item String -> Compiler (Item String)


data Template   = Archive
                | Default
                | Page
                | PostPage
                | PostWidget


templateFile :: Template -> Identifier
templateFile Archive    = "templates/archive.html"
templateFile Default    = "templates/default.html"
templateFile Page       = "templates/page.html"
templateFile PostPage   = "templates/postPage.html"
templateFile PostWidget = "templates/postWidget.html"


applyTemplate ::
    Template -> Context String -> Item String -> Compiler (Item String)
applyTemplate Default       = \ctx item ->  templateBody Default ctx item
                                            >>= relativizeUrls
applyTemplate Page          = inherit Default   Page
applyTemplate PostPage      = inherit Page      PostPage
applyTemplate Archive       = inherit Page      Archive
applyTemplate PostWidget    = templateBody PostWidget


inherit ::  Template -> Template -> HakyllTemplater
inherit parent = \tpl ctx item ->   templateBody tpl ctx item
                                    >>= applyTemplate parent ctx


templateBody :: Template -> HakyllTemplater
templateBody tpl = loadAndApplyTemplate $ templateFile tpl
