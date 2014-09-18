----
title: About modules
prevChapter: /en/miscellaneous/index.html
nextChapter: /en/miscellaneous/map.html
----

We've already know about modules, isn't it? Not completely. Let's dive into modules deeper.

## About hierarchy

Let's open `Helpers.hs` file from `src/Utils` directory and:

```haskell
module Utils.Helpers (
    calibrate,
    graduate
) where

coefficient :: Double

coefficient = 0.99874

calibrate length = length * coefficient

graduate length = length / coefficient
```

Now name of this module is `Utils.Helpers`, not `Helpers`, i.e. such a name is explicitly shows our sources' hierarchy. Strictly speaking, we're not need to do it, but such a practice is widespread. So now you know why modules we've worked with uses long names:

```haskell
import Data.String.Utils
```

It's just a path: there's a directory `Data` with a directory `String` with a file `Utils.hs`.

By the way, don't forget to change parameter `other-modules` in our `Real.cabal` file: replace `Helpers` to `Utils.Helpers`.

## About face

Every module have a face. Face is all stuff we can import to other modules. By default all content of the module is open, so we can import everything. But in real projects such a behaviour is not preferable.

Take a look at the beginning of `Utils.Helpers` module:

```haskell
module Utils.Helpers (
    calibrate,
    graduate
) where
```

Names of our two functions are placed in parentheses, and this is a face of this module: we may import only these two functions to other modules. Everything else in this module is inaccessible to the world. So if we write in some module this line:

```haskell
import Utils.Helpers
```

only two our functions can be used here, and nothing else. So if we want to share `coefficient` value between other modules - just add it in the face:

```haskell
module Utils.Helpers (
    calibrate,
    graduate,
    coefficient  -- Now it's visible to all.
) where
```

We can add our own type into the face:

```haskell
module Utils.Helpers (
    calibrate,
    graduate,
    Color (Red, Green, Blue)
) where

data Color = Red | Green | Blue deriving Show
```

Notice that it's not enough to place a name of type only, we must specify list of data constructors too.

## Nothing but...

In some cases we don't want import all stuff from module. Let's open Main.hs` and:

```haskell
import Utils.Helpers (calibrate)  -- Import calibrate only. 

main :: IO ()
main = print $ calibrate 12.4
```

We use a tuple after name of imported module, so now we can use only `calibrate` function, nothing else.

You may ask why would we need such a restriction? Main target of explicit specifying of imported names - avoidance of conflicts between names. In fact, many real modules contain stuff with the same names. In such cases we can import only what we really need.

## All but...

There's another variant of partial import: almost all. In this case we need a word `hiding`:

```haskell
import Utils.Helpers hiding (graduate)  -- Function graduate is hiding now.  
```

We specify names of hiding stuff in the tuple. This is used to avoiding of conflicts of names too.

## Belonging

In the real projects we'll often use same-name functions from the different modules. But we can't do it as usual, because compiler will not be able to understand who is where from. In such cases we should specify it explicitly:

```haskell
import Utils.Helpers -- Here is a function `calibrate`...
import Utils.Math    -- But what if here is a function with the same name? 

main :: IO ()
main = print $ Utils.Helpers.calibrate 12.4
```

We have specified module explicitly, so no conflicts will be here.

## Short belonging

Some packages (for example, known to us package `MissingH`) contain modules with very long names. For instance, `System.Console.GetOpt.Utils`. It's inconvenient to specify such a "prefix" every time. Fortunately, we can shorten name of module like this:

```haskell
import Utils.Helpers as H

main :: IO ()
main = print $ H.graduate 23
```

We use keyword `as` to create an alias for our module. By the way, name of alias must begin with a capital letter, so such a variant will be rejected:

```haskell
import Utils.Helpers as h
```

## Mandatory belonging

In some cases it's useful *to force* a developer to specify name of using modules. For example:

```haskell
import qualified Utils.Helpers as H 

main :: IO ()
main = print $ graduate 23
```

We use keyword `qualified`, so this code won't be compiled. Keyword `qualified` force us to specify name of module every time we use something from it:

```haskell
print $ H.graduate 23
```

## About Main module

And what about name of our `Main` module? Although name of this module can be omitted, you should name it (for unity with other modules):

```haskell
module Main where 

main :: IO ()
main = ...
```

In fact, `ghc` compiler *can* itself detect `Main` module, but standard Haskell 2010 recommend to specify name of `Main` module explicitly.

That's all. Now you know all about modules.

