{-
    Модуль, отвечающий за формирование базового контекста глав.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Context (
    chapterContext
) where

import Data.Monoid (mconcat)
import Misc        (aHost, bookName)
import Hakyll

-- Основной контекст глав.
chapterContext :: Context String
chapterContext = mconcat [ constField "host" aHost
                         , constField "bookName" bookName
                         , defaultContext
                         ]

