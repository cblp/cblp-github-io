module Local.Hakyll where

import Data.Binary      ( Binary )
import Data.Typeable    ( Typeable )
import Hakyll           ( Compiler
                        , Identifier
                        , Item
                        , Pattern
                        , Rules
                        , Writable
                        , compile
                        , compressCssCompiler
                        , copyFileCompiler
                        , create
                        , idRoute
                        , match
                        , route
                        , setExtension
                        , templateCompiler
                        )


cacheTemplates :: Pattern -> Rules ()
cacheTemplates pattern = compileFiles pattern templateCompiler


compileFilesExt ::  (Binary a, Writable a, Typeable a) =>
                    Maybe String -> Pattern -> Compiler (Item a) -> Rules ()
compileFilesExt mext pattern compiler =
    match pattern $ do
        route $ case mext of    Just ext    -> setExtension ext
                                Nothing     -> idRoute
        compile compiler


compileFiles :: (Binary a, Writable a, Typeable a) =>
                Pattern -> Compiler (Item a) -> Rules ()
compileFiles = compileFilesExt Nothing


compileFilesHtml :: (Binary a, Writable a, Typeable a) =>
                    Pattern -> Compiler (Item a) -> Rules ()
compileFilesHtml = compileFilesExt (Just "html")


compressCss :: Pattern -> Rules ()
compressCss pattern = match pattern $ do
    route idRoute
    compile compressCssCompiler


copyFiles :: Pattern -> Rules ()
copyFiles pattern = match pattern $ do
    route idRoute
    compile copyFileCompiler


createFile ::   (Binary a, Typeable a, Writable a) =>
                Identifier -> Compiler (Item a) -> Rules ()
createFile filename compiler =
    create [filename] $ do
        route idRoute
        compile compiler
