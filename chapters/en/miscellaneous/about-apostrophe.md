----
title: About apostrophe
prevChapter: /en/miscellaneous/recursive-functions.html
nextChapter: /en/miscellaneous/about-formatting.html
----

There's one naming convention in Haskell: name can contain apostrophe. Yes, it's a single quote we place a `Char` to.

We can use apostrophe in the name of the function:

```haskell
strangeFunction' :: Int -> Int
strangeFunction' arg = arg
```

in the name of type:

```haskell
data Strange_'type' = Strange_'type' String
```

in the name of type class:
 
```haskell
class Stran''geClass'' a where
    fmethod :: a -> String
```

and even in the name of value:
 
```haskell
strangeValue''' :: Integer
strangeValue''' = 123
```

In my opinion, apostrophe in the name is not so good idea. Moreover, apostrophe break a "word's unity" of the name. But there's a practice of usage of the apostrophe, so you should to know about it.

## Intermediate value

I mean such a case:

```haskell
let path = "/usr/local/"
    path' = path ++ "lib/"
```

I think it's better to write like this:

```haskell
let path = "/usr/local/"
    pathWithLib = path ++ "lib/"
```

## Math form

For example, derivative of function can be written like this:

```haskell
f' :: X -> Y
```

## Strict version

In many Hackage packages apostrophe is used at the end of name of the functions for strict version of these functions. For example, this is a lazy version:

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b 
```

and this is a strict version:

```haskell
foldl' :: (b -> a -> b) -> b -> [a] -> b 
```

That's all. Now you know about an apostrophe in the names.

