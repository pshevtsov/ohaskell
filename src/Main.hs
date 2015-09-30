{-
    Главный модуль.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Copiers              (justCopy, 
                             justCreateAndCopy,
                             justCompressAndCopy)
import Chapters             (createIndexPagesForEachLanguage,
                             createChaptersForEachLanguage,
                             createTableOfContentsForEachLanguage)
import Misc                 (prepareAllTemplates,
                             languages)
import IndexPage            (createIndexPage)
import Control.Monad.Reader (runReaderT)
import Hakyll

main :: IO ()
main = hakyll $ do
    justCopy            "static/images/*"
    justCompressAndCopy "static/css/*"
    justCompressAndCopy "static/js/*"
    justCopy            "README.md"
    justCopy            "CNAME"
    justCreateAndCopy   ".nojekyll"
    
    prepareAllTemplates

    -- Языки нужны всем, поэтому для удобства запускаем читателя.
    runReaderT (createIndexPage
                >> createIndexPagesForEachLanguage
                >> createChaptersForEachLanguage
                >> createTableOfContentsForEachLanguage) languages

