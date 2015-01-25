module Templates    ( Template(..)
                    , applyTemplate
                    )
where

import Control.Monad    ( (>=>) )
import Hakyll           ( Compiler
                        , Context
                        , Identifier
                        , Item
                        , fromFilePath
                        , loadAndApplyTemplate
                        , relativizeUrls
                        )


type HakyllTemplater = Context String -> Item String -> Compiler (Item String)


data Template   = Archive
                | Default
                | Page
                | PostPage
                | PostWidget
    deriving (Show)


templateFile :: Template -> Identifier
templateFile t = fromFilePath $ "templates/" ++ show t ++ ".html"


applyTemplate ::
    Template -> Context String -> Item String -> Compiler (Item String)
applyTemplate Default       = \ctx ->   templateBody Default ctx
                                        >=> relativizeUrls
applyTemplate Page          = inherit Default   Page
applyTemplate PostPage      = inherit Page      PostPage
applyTemplate Archive       = inherit Page      Archive
applyTemplate PostWidget    = templateBody PostWidget


inherit :: Template -> Template -> HakyllTemplater
inherit parent tpl ctx = templateBody tpl ctx >=> applyTemplate parent ctx


templateBody :: Template -> HakyllTemplater
templateBody tpl = loadAndApplyTemplate $ templateFile tpl
