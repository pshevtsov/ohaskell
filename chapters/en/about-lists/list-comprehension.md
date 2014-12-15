----
title: List comprehension
prevChapter: /en/about-lists/tuples.html
nextChapter: /en/about-user-types/index.html
----

So, let's talk about list comprehension. This tool is used for creating new lists based on old ones. It similar to functions `map` and `filter`, but more powerful.

## Intricate list

Here's how it looks:

```haskell
import Data.Char 

main = print [toUpper c | c <- "http"]
```

Result:

```bash
"HTTP"
```

So:

```haskell
[toUpper c | c <- "http"]
```

We see the square brackets... So is it a list? Not quite. We can say that it's a *generator* of the list. Skeleton of such a structure can be expressed as follows:

    [OPERATION ELEM | ELEM <- LIST] 

where `LIST` - a list, `ELEM` - element of this list, `OPERATION` - function that will be applied to each elements of this list. We say: "Take a list `LIST` and sequentially apply function `OPERATION` to each elements." And from a values returned by function `OPERATION` we create a new list.

In this case we sequentially apply function `toUpper` to each char in string `"http"`. As a result we get a new string `"HTTP"`.

## Add predicate

We can add a predicate in this structure. So skeleton will be:

    [OPERATION ELEM | ELEM <- LIST, PREDICATE]

In this case we say: "Take a list `LIST` and sequentially apply function `OPERATION` to elements that satisfy to a predicate `PREDICATE`".

For example:

```haskell
import Data.Char

main = print [toUpper c | c <- "http", c == 't']
```

Result:

```bash
"TT"
```

Function `toUpper` was applied only to `t` char. By the way, we may have more than one predicate. For example:

```haskell
import Data.Char

main = print [toUpper c | c <- "http", c /= 'h', c /= 'p']
```

Result is:
 
```bash
"TT"
```

Predicates `c /= 'h'` and `c /= 'p'` are merged via logical "AND", so we can write like this:

```haskell
[toUpper c | c <- "http", c /= 'h' && c /= 'p']
```

Result will be the same.

Take a look at `/=`. It's an operator for non-equality checking, like an operator `!=` in C langauge. By the way, compare these forms:

    /=   -- Haskell-form
    ≠    -- mathematical form

The direct affinity, we just shift a slash. 

## More lists 

We can work with more than one list at a time. So skeleton will be:

    [OPERATION_with_ELEMs | ELEM1 <- LIST1, ..., ELEMN <- LISTN ] 

We work with `N` lists, and function `OPERATION_with_ELEMs` applies to each element of all these lists. For example:

```haskell
main =
    print [prefix ++ name | name <- names, prefix <- namePrefix]
    where names = ["James", "Victor", "Denis", "Michael"]
          namePrefix = ["Mr. "]
```

Output is:

```bash
["Mr. James","Mr. Victor","Mr. Denis","Mr. Michael"]
```

In this example we work with two lists, `names` and `namePrefix`. Let's add second predicate into list `namePrefix`:

```haskell
main =
    print [prefix ++ name | name <- names, prefix <- namePrefix]
    where names = ["James", "Victor", "Denis", "Michael"]
          namePrefix = ["Mr. ", "sir "]  -- Теперь префиксов два
```

In this case output is:

```bash
["Mr. James","sir James","Mr. Victor","sir Victor","Mr. Denis","sir Denis","Mr. Michael","sir Michael"]
```

As you see we work with *each* element of both lists.

## Add condition

In some cases predicate is not what we need. Sometimes we need a condition. Let's add it:

```haskell
main =
    print [if car == "Bentley" then "Wow!" else "Good!" | car <- cars]
    where cars = ["Mercedes",
                  "BMW",
                  "Bentley",
                  "Audi",
                  "Bentley"]
```

Result:

```bash
["Good!","Good!","Wow!","Good!","Wow!"]
```

Here we apply a condition to each string.

## Add local expression

We can add a local expression using `let` keyword. For example:

```haskell
import Data.Char

main = print [toUpper c | c <- "http",
                          let hletter = 'h' in c /= hletter]
```

As usual, local expression can be used to avoid code duplication.

## One more example

Well, it's a little more practical example:

```haskell
import Data.List

checkGooglerBy :: String -> String
checkGooglerBy email =
    if "gmail.com" `isSuffixOf` email
    then nameFrom email ++ " is a Googler!"
    else email
    where nameFrom fullEmail = takeWhile (/= '@') fullEmail

main = print [checkGooglerBy email | email <- ["adam@gmail.com",
                                               "bob@yahoo.com",
                                               "richard@gmail.com",
                                               "elena@yandex.ru",
                                               "denis@gmail.com"]]
```

Result is:

```haskell
["adam is a Googler!","bob@yahoo.com","richard is a Googler!","elena@yandex.ru","denis is a Googler!"]
```

Take a look at this line:

```haskell
takeWhile (/= '@') fullEmail
```

Skeleton of standard function `takeWhile` looks like this:

    takeWhile PREDICATE LIST 

We say: "Take elements from list `LIST` as long as predicate `PREDICATE` we apply to element returns `True`. As soon as meet an element that doesn't satisfy to `PREDICATE` - immediately stop working and return the new list." So we find `'@'` char and take substring from chars before it, and this is a user name.

## So

* List comprehension automatically generates a new list based on existing list(s). 
* A new list is generated by applying some functions to an elements of existing list(s). 


