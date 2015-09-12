----
title: Кортежи
prevChapter: /ru/about-lists/ranges.html
nextChapter: /ru/about-lists/list-comprehension.html
----

Воспринимайте кортеж как особый вид списка. Он тоже хранит в себе набор элементов, однако имеет три фундаментальных отличия от списка:

1. круглые скобки вместо квадратных,
2. гетерогенность,
3. тип, зависящий от размера.

С первым отличием всё очевидно:

```haskell
["Denis", "Shevchenko"]  -- Это список из двух строк.
("Denis", "Shevchenko")  -- Это кортеж из двух строк.
```

Второе отличие - это способность кортежа хранить в себе элементы разных типов:

```haskell
["Denis", 1.234]  -- Компиляция не пройдёт...
("Denis", 1.234)  -- А тут - без проблем!
```

Теперь о третьем отличии. Если у нас есть два разных по размеру списка строк:

```haskell
["Denis", "Vasil`evich", "Shevchenko"]
["Denis", "Shevchenko"]
```

тип обоих этих списков одинаков, а именно `[String]`. Тип списка не зависит от количества элементов в нём.

С кортежами всё обстоит совершенно иначе. Если у нас есть два кортежа, разных по длине:

```haskell
("Denis", "Vasil`evich", "Shevchenko")
("Denis", "Shevchenko")
```

типы этих кортежей абсолютно разные: тип первого - `(String, String, String)`, а тип второго - `(String, String)`. Поэтому если функцию, в качестве аргумента ожидающую кортеж из двух строк, применить к кортежу из трёх строк, компилятор выразит свой категорический протест:

```bash
Couldn't match expected type `(String, String)'
            with actual type `([Char], [Char], [Char])'
```

Оно и понятно: ожидали кортеж из двух строк, а тут вдруг - из трёх!

Кстати, кортеж похож на список ещё и тем, что тоже может быть пустым, то есть не содержать в себе ни одного элемента. Пустой кортеж используется как отражение понятия "ничего". Скоро мы увидим такие случаи.

## Что с ними можно делать

Единственное, что можно сделать с кортежем - извлечь хранящиеся в нём элементы. Всё.

На практике чаще всего используют кортежи из двух элементов. Такой кортеж ещё называют парой (pair). Чтобы извлечь хранящиеся в нём элементы, используют стандартные функции `fst` и `snd`.

Определим функцию, применяющуюся к кортежу, хранящему две части шахматного хода:

```haskell
chessMove :: (String, String) -> String
chessMove pair = fst pair ++ "-" ++ snd pair

main = print $ chessMove ("e2", "e4")
```

Мы последовательно извлекли первый и второй элементы из полученной пары и сделали из них единую строку:

```bash
e2-e4
```

Всё предельно просто. Но что же мы будем делать, если количество элементов в кортеже окажется больше двух? Ведь функции `fst` и `snd` работают только с парами. Если элементов больше двух, извлекать их нужно иным способом.

## Неудобный способ

Первый способ неудобен, ибо нам придётся самим определять необходимые функции. Но нас трудности не страшат, поэтому сделаем это:

```haskell
get1 (element, _, _, _) = element
get2 (_, element, _, _) = element
get3 (_, _, element, _) = element
get4 (_, _, _, element) = element
```

Подразумевается, что мы хотим работать с кортежем из четырёх элементов. В этом случае есть лишь четыре варианта извлечения, поэтому определим четыре функции для извлечения первого элемента, второго, третьего и четвёртого соответственно. Кстати, говоря "первый элемент", мы подразумеваем именно первый по счёту, поэтому цифра `1` в имени `get1` - это порядковый номер, а не индекс.

А теперь рассмотрим определение первой функции:

```haskell
get1 (element, _, _, _) = element
```

Эта функция применяется к кортежу из четырёх элементов и возвращает первый из них. Обратите внимание на странные символы подчёркивания. Воспринимайте этот символ как "нечто", "что бы то ни было". Эдакий placeholder-пустышка. Мы говорим: "Да, в этом кортеже есть четыре элемента, но нас абсолютно не интересует, что там под номером два, и что под номером три, и что под номером четыре. Нас интересует только то, что под номером один. Вот этот номер один мы и вернём".

Так же и вторая функция:

```haskell
get2 (_, element, _, _) = element
```

Получаем четыре элемента, и хотя что-то там стоит под номерами один, три и четыре, нас это не волнует, нам нужен только элемент под номером два, поэтому именно его и возвращаем.

А теперь пишем:

```haskell
main = print $ get3 ("One", "Two", "Three", "Four")
```

и получаем ожидаемый результат:

```bash
"Three"
```

## Удобный способ

Зачем делать самому то, что уже сделали другие? А другие уже сделали пакет `tuple` из Hackage. Живёт пакет [здесь](http://hackage.haskell.org/package/tuple). Установим его командой:

```bash
$ stack install tuple
```

Как обычно, указываем этот пакет в сборочном файле `Real.cabal`, а теперь пишем:

```haskell
import Data.Tuple.Select

main = print $ sel3 ("One", "Two", "Three", "Four")
```

Метод `sel3` извлекает третий элемент кортежа. Просто и удобно. Кстати, в модуле `Data.Tuple.Select` определены функции от `sel1` до `sel15`. Авторы этого пакета вполне резонно предположили, что создавать кортеж из более чем 15 элементов никакому вменяемому программисту в голову не придёт...

А кстати, как насчёт безопасности? Что будет, если мы по ошибке попытаемся извлечь из этого кортежа пятый элемент? Попробуем:

```haskell
import Data.Tuple.Select

main = print $ sel5 ("One", "Two", "Three", "Four")
```

Итак, пытаемся извлечь пятый элемент при наличии только четырёх. Получили трудноуловимую ошибку? Или, может, будет брошено исключение? Вовсе нет. Такой код просто не пройдёт компиляцию:

```bash
src/Main.hs:23:12:
    No instance for (Sel5 ([Char], [Char], [Char], [Char]) a0)
      arising from a use of `sel5'
```

Как мы помним, тип кортежа жёстко привязан к количеству хранящихся в нём значений. Именно поэтому такого рода ошибки будут гарантированно выявлены на стадии компиляции.

## В сухом остатке

* Кортеж - это своего рода упрощённый список, позволяющий хранить значения разных типов.
* Тип кортежа определяется количеством и типом хранящихся в нём значений.
* Единственное, что мы можем сделать с кортежем - извлечь из него значения, если таковые имеются.