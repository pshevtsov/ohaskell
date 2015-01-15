#!/bin/bash

# Просто собираем веб-версию книги, локально.

set -e

cabal clean && cabal configure && cabal build

./dist/build/book/book rebuild

# После этого в корне репозитория смотрим в каталог _site.
