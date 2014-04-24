#!/bin/bash

# Создаём русскую версию ohaskell_ru.epub
gitbook ebook ../ru/ --title="О Haskell по-человечески" --output=./ohaskell_ru.epub

# Создаём русскую версию ohaskell_ru.mobi
ebook-convert ohaskell_ru.epub ohaskell_ru.mobi
