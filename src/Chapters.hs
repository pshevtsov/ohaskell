{-
    Модуль, отвечающий за преобразование глав и в формирование корректных путей к ним.
    https://github.com/denisshevchenko/ohaskell
    Все права принадлежат Денису Шевченко и сообществу, 2013-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Chapters (
    createIndexPagesForEachLanguage,
    createChaptersForEachLanguage,
    createTableOfContentsForEachLanguage
) where

import Context              (chapterContext)
import Misc                 (LanguagesReader)
import Control.Monad.Reader
import Hakyll

removeChaptersDirectoryFromURLs :: Routes
removeChaptersDirectoryFromURLs = gsubRoute "chapters/" (const "")

createIndexPageFor :: String -> Rules ()
createIndexPageFor languageMark =
    let markdownPage = fromGlob $ "chapters/" ++ languageMark ++ "/index.md"
        templateName = fromFilePath $ "templates/" ++ languageMark ++ "_default.html"
    in
    match markdownPage $ do
        route $ removeChaptersDirectoryFromURLs `composeRoutes` 
                setExtension "html"
        compile $ pandocCompiler >>= loadAndApplyTemplate templateName chapterContext
                                 >>= relativizeUrls

createIndexPagesForEachLanguage :: LanguagesReader
createIndexPagesForEachLanguage = do
    languages <- ask
    lift $ mapM createIndexPageFor languages
    return ()

createChaptersFor :: String -> Rules ()
createChaptersFor languageMark =
    let chapters = fromGlob $ "chapters/" ++ languageMark ++ "/**"
        chapterTemplateName = fromFilePath $ "templates/" ++ languageMark ++ "_chapter.html"
        defaulTemplateName = fromFilePath $ "templates/" ++ languageMark ++ "_default.html"
    in
    match chapters $ do
        route $ removeChaptersDirectoryFromURLs `composeRoutes`
                setExtension "html"
        compile $ pandocCompiler >>= loadAndApplyTemplate chapterTemplateName chapterContext
                                 >>= loadAndApplyTemplate defaulTemplateName chapterContext
                                 >>= relativizeUrls

createChaptersForEachLanguage :: LanguagesReader
createChaptersForEachLanguage = do
    languages <- ask
    lift $ mapM createChaptersFor languages
    return ()

createTableOfContentsFor :: String -> Rules ()
createTableOfContentsFor languageMark =
    let markdownPage = fromGlob $ "chapters/" ++ languageMark ++ "_chapters.md"
        defaulTemplateName = fromFilePath $ "templates/" ++ languageMark ++ "_default.html"
        routePattern = "chapters/" ++ languageMark ++ "_"
        languageSubDirectory = languageMark ++ "/"
    in
    match markdownPage $ do
        route $ gsubRoute routePattern (const languageSubDirectory) `composeRoutes`
                setExtension "html"
        compile $ pandocCompiler >>= loadAndApplyTemplate defaulTemplateName chapterContext
                                 >>= relativizeUrls

createTableOfContentsForEachLanguage :: LanguagesReader
createTableOfContentsForEachLanguage = do
    languages <- ask
    lift $ mapM createTableOfContentsFor languages
    return ()

