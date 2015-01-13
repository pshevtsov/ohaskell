{-
    Вспомогательный модуль.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Misc (
    aHost,
    prepareAllTemplates,
    LanguagesReader,
    bookName,
    languages
) where

import Control.Monad.Reader
import Hakyll

-- Данный URL останется актуальным до тех пор, пока сайт будет жить на GitHub Pages.
aHost :: String
aHost = "http://ohaskell.dshevchenko.biz"

-- Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

bookName :: String
bookName = "О Haskell по-человечески"

-- Читательское "облако" с поддерживаемыми языками.
type Languages = [String]
type LanguagesReader = ReaderT Languages Rules ()

languages :: Languages
languages = ["ru", "en"]

