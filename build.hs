#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Development.Shake


main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["_site"]

    "_site" ~> jekyll_build

    where
        jekyll_build = cmd "jekyll" "build"
