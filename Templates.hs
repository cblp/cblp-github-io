module Templates  ( Template(..)
                  , applyTemplate
                  )
where

import Hakyll     ( Compiler
                  , Context
                  , Identifier
                  , Item
                  , fromFilePath
                  , loadAndApplyTemplate
                  , relativizeUrls
                  )


type HakyllTemplater = Context String -> Item String -> Compiler (Item String)


data Template = Archive
              | Content
              | Default
              | Page
              | PostPage
              | PostWidget
    deriving (Show)


templateFile :: Template -> Identifier
templateFile t = fromFilePath $ "templates/" ++ show t ++ ".html"


applyTemplate :: Template -> HakyllTemplater
applyTemplate Archive     = templateBody Archive  & wrap Content & wrap Page
applyTemplate Content     = templateBody Content
applyTemplate Default     = \ctx item ->  templateBody Default ctx item
                                          >>= relativizeUrls
applyTemplate Page        = templateBody Page     & wrap Default
applyTemplate PostPage    = templateBody PostPage & wrap Content & wrap Page
applyTemplate PostWidget  = templateBody PostWidget


wrap :: Template -> HakyllTemplater -> HakyllTemplater
wrap parent = \tplr ctx item ->  tplr ctx item
                                    >>= applyTemplate parent ctx


templateBody :: Template -> HakyllTemplater
templateBody tpl = loadAndApplyTemplate $ templateFile tpl


(&) :: a -> (a -> b) -> b
x & f = f x
