----
title: Handling exception
prevChapter: /en/io/do-imperative-world.html
nextChapter: /en/io/own-exceptions.html
----

We all aim at creating error-free software. Nevertheless, they sometimes emerge, and therefore, we have to deal with them. Let's discuss exceptions, that topic is especially necessary to know as many packages from Hackage contain code which throws exceptions.

First of all, we need module `Control.Exception`:

```haskell
import Control.Exception
```

That standard module is made for throwing and catching exceptions, either standard ones or our own. It's significant that we can throw exception from either a pure function or from a function with side effect, but we can't catch it in pure function.

## Problem with file

Usually we'll be meeting exceptions which are thrown from functions which interact with the outside world. Canonical example: we want to read the content of the non-existent file:

```haskell
main :: IO ()
main = do
    fileContent <- readFile "Users/shevchenko/test.c"  -- Incorrect path...
    putStrLn fileContent
```

That gives us output:

```bash
Real: Users/shevchenko/test.c: openFile: does not exist (No such file or directory)
```

Function `readFile` had thrown an exception, as it's the only way it could report about a problem with file. Exception haven't found any barriers on its way and was caught, on application's top level, after that a message about error was printed out and the application died on its feet.

## Catching

So:

```haskell
import Control.Exception 

tryToOpenFile :: FilePath -> IO String
tryToOpenFile path =
    readFile path `catch` possibleErrors
    where
        possibleErrors :: IOException -> IO String
        possibleErrors error = return $ show error

main :: IO ()
main = do
    fileContent <- tryToOpenFile "Users/shevchenko/test.c"
    putStrLn fileContent
```

Now we have a function `tryToOpenFile`, which opens file at given path, but does it carefully, utilizing function `catch`. As you already understood, function `catch` (defined in module `Control.Exception`) knows how to catch an exception. Consider its type:

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

Function takes two arguments: first is an `IO`-action, second is a handling-function. If action thrown an exception (represented with polymorphic type `e`) that exception is passed to the handler.

Watch out: the type of exception is in the context of `Exception` class, which is defined in the same module `Control.Exception`. Any exception in our application must implement that class.

In order to improve readability we use function `catch` in infix notation, which keeps up with the name of our handler:

```haskell
readFile path `catch` possibleErrors
```

If we get an error while reading a file the corresponding exception is passed to our handler:

```haskell
possibleErrors :: IOException -> IO String
possibleErrors error = return $ show error
```

Handler takes value of type `IOException`. In the handler's body we stringify the resultant exception, then we take the resultant string containing a description of an error and wrap it into an action `IO String`. In case if you are not happy with existing error message provide your own:

```haskell
possibleErrors error = return "Unable to open this file. Please check it."
```

That message would be printed if the problem occurs.

## Catch the other way

Let's rewrite our function:

```haskell
tryToOpenFile :: FilePath -> IO String
tryToOpenFile path =
    handle possibleErrors (readFile path)  -- Same thing but the other way.
    where
        possibleErrors :: IOException -> IO String
        possibleErrors error = return "Aaaaa!!! Please check file."
```

We replaced function `catch` with `handle`. There is absolutely no difference, except the order of arguments `catch` takes handler as a second argument, whereas `handle` takes it as first. Thus, `catch` is more readable in infix form and `handle` is more readable in simple form. So, take your choice.

## Trying

Standard function `try` takes different approach. For example:

```haskell
import Control.Exception

main :: IO ()
main = do
    result <- try $ readFile path :: IO (Either IOException String)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right content -> putStrLn content
    where path = "Users/dshevchenko/test.c"
```

Let's line it out.

Here is a type of function `try`:

```haskell
try :: Exception e => IO a -> IO (Either e a)
```

That function takes our `IO`-action and returns another `IO`-action, which returns value of standard type `Either e a`. `Either` is a type constructor designed to store either value which represents good result or value which represents bad result. Note, we explicitly specified type of value which function `try` returns.

```haskell
try $ readFile path :: IO (Either IOException String)
```

We said, "Let function `try` return us a value of type `Either IOException String`, which contains either value of type `IOException` (when something happened while reading a file) or a value of type `String` with the contents of file".

Next, check was it successful:

```haskell
case result of
    Left exception -> putStrLn $ "Fault: " ++ show exception
    Right content -> putStrLn content
```

Type `Either` has two constructors, `Left` and `Right`. In our case it can be displayed it this way:

    Either IOException String
           |           |
           Left        Right

Using these constructors we can comprehend what happened. Structure `case-of` will help us. We say, "If `result` conforms to left value it's a value of type `IOException`. Something went wrong, print an exception! And if `result` conforms to right value -- we've got `String`. Great success, print the contents of the file."

## In the Pure World

Sometimes we need to catch an exception which was thrown from pure function. For example:

```haskell
import Control.Exception

main :: IO ()
main = do
    result <- try $ 2 `div` 0 :: IO (Either SomeException Integer)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right value -> print value
```

Here we tried to check the result of dividing 2 by zero. However, that wouldn't pass the compilation. Function `try` expects to get `IO`-action on input, however standard function `div` is pure and returns normal number. Therefore, we need a small trick:

```haskell
import Control.Exception

main :: IO ()
main = do
    result <- try $ evaluate $ 2 `div` 0
                    :: IO (Either SomeException Integer)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right value -> print value
```

We wrapped the call to function `div` into a standard function `evaluate`. Now everything compiles, and on execution we get an expected angry message:

```bash
Fault: divide by zero
```

The type of function `evaluate`:

```haskell
evaluate :: a -> IO a
```

That function plays the role of adapter: it kind of turns the result into an `IO`-action which returns that result. After function `div` returned us a normal number function `evaluate` wrapped that number into an action expected by function `try`.

## So

* Exception can be thrown from any function, but we can catch it only inside IO-function.
* Function `catch` catches exception and passes it to the handler.
* Function `handle` - is a mirroring of `catch`.
* Function `try` co-operates with constructor of type `Either`. Analysing the value created by that type we can understand if there was a problem in the particular function or it went smooth.
* In order to work with `try` we should put pure function into `evaluate`-wrapper

Almost done. However, we definitely should take a look on how to create our own exceptions.
