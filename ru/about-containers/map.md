Словарь
-------

Ассоциативный контейнер, часто называемый словарём (dictionary), давно стал классикой среди гомогенных контейнеров. Естественно, в Haskell-проектах он используется весьма часто. Впрочем, если вдруг вы запамятовали, о чём идёт речь, напомню, что словарём называется контейнер, хранящий в себе пары "ключ-значение", и позволяющий нам удобно работать с этими самыми ключами и значениями.

Нам понадобится стандартный пакет `containers`, однако ставить его вам не придётся, поскольку он давно подтянулся в зависимостях других пакетов.

### Проба пера

Итак, пишем:

```haskell
module Main where

import qualified Data.Map.Lazy as M

main :: IO ()
main = 
    putStrLn $ case M.lookup "Toyota" cars of
        Nothing -> "There's no cars from Toyota, sorry..."
        Just aCars -> show aCars
    where cars = M.fromList [
                              ("Toyota", ["Camry", "Corolla", "Prius"]),
                              ("Nissan", ["Juke", "Almera"]),
                              ("Honda",  ["Civic", "CR-V", "Accord", "CR-Z"])
                            ]    
```

Результат:

```bash
["Camry","Corolla","Prius"]
```

Вначале мы создаём словарь из списка пар, с помощью функции `fromList`. Кстати, обратите внимание, что модуль `Data.Map.Lazy` включён как `qualified`, что вынудит нас явно указывать принадлежность. Связано это с тем, что имена многих функций модуля `Data.Map.Lazy` конфликтуют с функциями из `Prelude`.

Как вы уже поняли, функция `fromList` создала словарь с тремя ключами (марками автомобилей), каждому из которых соответствует список строк (моделей данной марки). А функция `lookup` позволила нам найти все марки от Toyota, по соответствующему ключу.

### Добавляем новое

Словарь позволяет легко добавлять новые пары:

```haskell
module Main where

import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)

main :: IO ()
main = 
    let cars = M.empty
        carsWithToyota = M.insert "Toyota" ["Corolla", "Prius"] cars
    in
    print $ fromJust $ M.lookup "Toyota" carsWithToyota
```

Функция `empty` создаёт девственно чистый словарь `cars`, а затем мы вставляем в него список моделей Toyota, с помощью функции `insert`. Разумеется, если бы в словаре *уже* хранился список, соответствующий ключу `"Toyota"`, он был бы перезаписан новым списком.

### Добавляем к новому

А что если мы захотим добавить ещё одну модель к уже имеющемуся списку моделей от Toyota? Никаких проблем:

```haskell
module Main where

import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust) 

type Brand = String
type Model = String
type Cars = M.Map Brand [Model]

addNewModel :: Brand -> Model -> Cars -> Cars
addNewModel brand model cars = M.insertWith (++) brand [model] cars

main :: IO ()
main = 
    let cars = M.fromList [("Toyota", ["Corolla", "Prius"])]
        carsWithBigGuy = addNewModel "Toyota" "Sequoia" cars
    in
    print $ fromJust $ M.lookup "Toyota" carsWithBigGuy
```

Вывод:

```bash
["Sequoia","Corolla","Prius"]
```

Чтобы предназначение функции `insertWith` было предельно понятным, мы обёрнули её в функцию `addNewModel`. Берём новую модель Toyota - и добавляем её к списку уже имеющихся моделей.

Вот как это работает:

```haskell
M.insertWith (++) brand [model] cars
```

Здесь мы говорим: "Найди в словаре `cars` список моделей, соответствующих ключу `brand`, а потом прибавь к этому списку новую модель в виде `[model]`". Оператор конкатенации списков применяется к новому значению и к старому значению (если таковое имеется), поэтому работа фунции `insertWith` сводится в конечном итоге к простому объединению:

    ["Sequoia"]         ++        ["Corolla", "Prius"]
    новое значение      бинарная  старое значение
    для ключа "Toyota"  функция   для ключа "Toyota"
    
Вот ещё один пример использования функции `insertWith`:

```haskell
module Main where

import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust) 
import Text.Printf (printf)

type Price = Double
type Difference = Double
type PriceName = String
type Prices = M.Map PriceName Price

increase :: PriceName -> Difference -> Prices -> Prices
increase name difference prices = M.insertWith (+) name difference prices

main :: IO ()
main = 
    let prices = M.fromList [   
                              ("Month price", 12.3),
                              ("Year price", 120.0)
                            ]
        newPrices = increase "Month price" 2.0 prices
    in
    printf "%f" $ fromJust $ M.lookup "Month price" newPrices
```

Есть словарь различных цен, и мы, используя функцию `insertWith`, увеличиваем месячную цену путём применения оператора сложения к старому и новому значениям:

    2.0             +         12.3 
    новое значение  бинарная  старое значение
    для ключа       функция   для ключа 
    "Month price"             "Month price"

Кстати, вы заметили? Привычная функция `print` заменена на не очень привычную `printf`. Сразу повеяло языком C, не правда ли? Эта функция живёт в стандартном модуле `Text.Printf` и предназначена для форматной печати значений. Да-да, это клон той самой C-шной `printf`. Без неё значение типа `Double` было бы выведено с большим количеством ненужных нулей. Подробнее о форматной печати читайте [здесь](http://hackage.haskell.org/package/base/docs/Text-Printf.html).

### Обновляем

Как уже было сказано выше, значение, ассоциированное с ключом, можно обновить с помощью функции `insert`. Однако есть и другие способы. 

Помните, как мы добавили новую модель к уже имеющимся моделям Toyota? 

```haskell
addNewModel :: Brand -> Model -> Cars -> Cars
addNewModel brand model cars = M.insertWith (++) brand [model] cars
```

`"Sequoia"` добавилась в начало списка, но что если мы хотим добавить её в конец? Используем функцию `adjust`:

```haskell
addNewModel :: Brand -> Model -> Cars -> Cars
addNewModel brand model cars = M.adjust (++ [model]) brand cars
```

Вывод:

```bash
["Corolla","Prius","Sequoia"] 
```

Разберёмся, как это работает. Вот объявление функции `adjust`:

```haskell
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
```

Значение для ключа `k` будет заменено результатом применения унарной функции к старому значению, ассоциированному с ключом `k`. И чтобы стало совсем понятно, заменим реальными значениями:

```haskell
M.adjust (++ ["Sequoia"]) "Toyota" cars
```

Теперь всё предельно понятно: взяли список моделей, уже ассоциированный с ключом `"Toyota"`, и применили к нему частично применённый оператор конкатенации (ставший унарной функцией), в результате чего `"Sequoia"` добавляется в конец списка имеющихся моделей. 

### Удаляем

Если нам нужно удалить конкретный ключ и связанное с ним значение, воспользуемся функцией `delete`:

```haskell
module Main where

import qualified Data.Map.Lazy as M

main :: IO ()
main =
    let cars = M.fromList [
                            ("Toyota", ["Camry", "Corolla", "Prius"]),
                            ("Nissan", ["Juke", "Almera"]),
                            ("Honda",  ["Civic", "CR-V", "Accord", "CR-Z"])
                          ]
        carsWithoutHonda = M.delete "Honda" cars
    in
    putStrLn $ case M.lookup "Honda" carsWithoutHonda of
        Nothing -> "There's no cars from Honda, sorry..."
        Just aCars -> show aCars
```

Передаём функции `delete` ключ и словарь - готово.

### Пробегаемся

С помощью собственной функции `map` можно пробежаться по всем значениям в словаре и сделать с ними что-нибудь интересное. Например:

```haskell
module Main where

import qualified Data.Map.Lazy as M
import Data.List (isPrefixOf)

type Url = String

correctAll :: [Url] -> [Url]
correctAll urls = 
    map correctOne urls
    where correctOne url | "http" `isPrefixOf` url = url
                         | otherwise = "http://" ++ url

main :: IO ()
main =
    let urls = M.fromList [
                ("Maps",      ["maps.yandex.ru", "google.com/maps"]),
                ("Translate", ["http://slovari.yandex.ru", "translate.google.com"]),
                ("Cloud",     ["https://drive.google.com", "https://disk.yandex.ru"])
                          ]
        correctedUrls = M.map correctAll urls
    in
    print $ M.elems correctedUrls
```

Вывод будет таким:

```bash
[["https://drive.google.com","https://disk.yandex.ru"],["http://maps.yandex.ru","http://google.com/maps"],["http://slovari.yandex.ru","http://translate.google.com"]]
```

В словаре имеются группы ссылок, но некоторые из них без префикса. Мы прогоняем функцию `correctAll` через все значения в словаре (то есть через все списки ссылок), а внутренняя функция `correctOne` применяется к каждой из ссылок, добавляя префикс, если таковой отсутствует. В конце, используя функцию `elems`, мы выводим список из всех значений словаря, минуя ключи.

### И так далее

Да, там ещё [много вкусностей](http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html), рассматривать их всех мы не станем. Главное, что стандартный ассоциативный контейнер, как мы убедились, штука весьма удобная и простая в использовании.

### Об эффективности

В целом `Data.Map` работает шустро. Однако существует специализированная версия по имени `Data.IntMap`. Как понятно из названия, данная версия может использоваться только в тех случаях, когда ключи имеют тип `Int`. Вы спросите, зачем же нужна эта версия, если в `Data.Map` тоже можно запихивать целочисленные ключи? Причина в эффективности: как утверждает автор пакета `containers`, `Data.IntMap` значительно шустрее, особенно когда речь идёт о вставке и удалении пар.

Так что если вы имеете дело с целочисленными ключами и вам очень уж важна скорость работы со словарём - используйте `Data.IntMap`. Причём, что удобно, набор функций, работающих с `Data.IntMap`, практически такой же, как и для `Data.Map`. Поэтому при переходе на высокоэффективную версию вам не потребуется серьёзно изменять свой код.

### В сухом остатке

* Словарь хранит в себе пары "ключ-значение" и позволяет совершать различные полезные действия с этими ключами и значениями. 
* Для высокоэффективной работы со словарями, в которых используются целочисленные ключи, рекомендуется использовать `Data.IntMap`.

### Пробуем

Код из этой главы доступен онлайн.

<span><a href="https://www.fpcomplete.com/ide?title=applicative-functors&paste=https://raw.githubusercontent.com/denisshevchenko/ohaskell-code/master/code/about-containers/map/Main.hs" class="fpcomplete_code" target="_blank">Открыть в FP IDE</a></span>
<span class="buttons_space"></span>
<span><a href="https://github.com/denisshevchenko/ohaskell-code/blob/master/code/about-containers/map/Main.hs" class="github_code" target="_blank">Открыть на GitHub</a></span>

