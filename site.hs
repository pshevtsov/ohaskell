{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)

import Hakyll

hostOfBook :: String
hostOfBook = "http://ohaskell.dshevchenko.biz"

bookName :: String
bookName = "О Haskell по-человечески"

languages :: [String]
languages = ["ru", "en"]

main :: IO ()
main = hakyll $ do
    -- Просто копируем все изображения из корневого каталога images...
    match "images/*" justCopy
    
    -- Просто копируем все стили из корневого каталога css...
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler 

    -- Просто копируем файл README
    match "README.md" justCopy
    
    -- Просто копируем файл CNAME, необходимый для 
    -- поддержки собственного dns на GitHub Pages...
    match "CNAME" justCopy

    -- Создаём файл .nojekyll и просто копируем его.
    -- Он необходим для того, чтобы сообщить GitHub Pages
    -- о том, что этот сайт (к сожалению или к счастью) не на Jekyll...
    create [".nojekyll"] justCopy

    -- Создаём .htaccess и применяем к нему заготовленный шаблон...
    create [".htaccess"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/htaccess" defaultContext

    -- Создаём страницу 404 и применяем к ней шаблон стандартной страницы, а также собственную заготовку...
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/404.html" chapterContext
                              >>= loadAndApplyTemplate "templates/ru_default.html" chapterContext
                              >>= relativizeUrls
    
    -- Обрабатываем главную страницу книги...
    match "index.md" $ do
        route $ setExtension "html"
        -- Используем pandocCompiler, потому что главная страница написана
        -- на Markdown, и pandoc необходим, чтобы превратить её в html...
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/init.html" chapterContext
                                 >>= relativizeUrls
    
    mapM prepareMainPageFor languages
    
    mapM prepareChaptersFor languages
    
    mapM prepareTableOfContentsFor languages
    
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

justCopy :: Rules ()
justCopy = do
    route   idRoute
    compile copyFileCompiler

prepareMainPageFor :: String -> Rules ()
prepareMainPageFor languageMark =
    let markdownPage = fromGlob $ "chapters/" ++ languageMark ++ "/index.md"
        templateName = fromFilePath $ "templates/" ++ languageMark ++ "_default.html"
    in
    match markdownPage $ do
        route $ removeChaptersDirectoryFromURLs `composeRoutes`
                setExtension "html"
        compile $ pandocCompiler >>= loadAndApplyTemplate templateName chapterContext
                                 >>= relativizeUrls

prepareChaptersFor :: String -> Rules ()
prepareChaptersFor languageMark =
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

prepareTableOfContentsFor :: String -> Rules ()
prepareTableOfContentsFor languageMark =
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

removeChaptersDirectoryFromURLs :: Routes
removeChaptersDirectoryFromURLs = gsubRoute "chapters/" (const "")

chapterContext :: Context String
chapterContext = mconcat [ constField "host" hostOfBook
                         , constField "bookName" bookName
                         , defaultContext
                         ]

