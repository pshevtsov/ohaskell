{-
    Модуль, отвечающий за формирование главной страницы.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module IndexPage (
    createIndexPage
) where

import Misc                 (LanguagesReader)
import Control.Monad.Reader
import Hakyll

createIndexPage :: LanguagesReader
createIndexPage = do
    lift $ create ["index.html"] $ do
        route idRoute
        compile $ do
            let indexContext = mconcat [ constField "title" "#ohaskell"
                                       , defaultContext
                                       ]
            
            makeItem "" >>= loadAndApplyTemplate "templates/init.html" indexContext
                        >>= relativizeUrls
    return ()

