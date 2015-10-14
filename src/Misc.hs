{-
    Вспомогательный модуль.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Misc (
    prepareAllTemplates,
    LanguagesReader,
    languages
) where

import Control.Monad.Reader
import Hakyll

-- Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

-- Читательское "облако" с поддерживаемыми языками.
type Languages = [String]
type LanguagesReader = ReaderT Languages Rules ()

languages :: Languages
languages = ["ru", "en"]

