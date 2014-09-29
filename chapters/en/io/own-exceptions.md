----
title: Own exceptions
prevChapter: /en/io/exceptions-handling.html
nextChapter: /en/delicious/index.html
----

Previously we were only catching exceptions. Now, let's discuss how to throw them and how to create our own exception types.

## Creating

Define the type of exception:

```haskell
import Control.Exception
import Data.String.Utils
import Data.Typeable

type Repo = String

data InvalidRepository = InvalidRepository Repo
                         deriving (Show, Typeable)

instance Exception InvalidRepository
```

Some repository is analyzed, if it is incorrect it's time for exception `InvalidRepository`. This exception is inherited from two type-classes, `Show` and `Typeable`. That's necessary as our type must provide an instance of type-class `Exception` and that type class deduces context from these two type-classes.

You'll ask, what is that unusual string for:

```haskell
instance Exception InvalidRepository
```

We face an instance of type-class `Exception` but that instance has neither keyword `where`, nor subsequent implementations of corresponding methods. Type-class `Exception` contains two different methods, but we say, "Here is our instance of type-class `Exception`, but we are not going to provide implementations of its methods".

## Throw

In order to throw the exception we use standard function `throw`. Remember that even in case if exception was thrown from pure function, we can catch it only in `IO`-function.

Write:

```haskell
extractProtocol :: String -> String
extractProtocol path =
    if path `startsWith` "git" || path `startsWith` "ssh"
    then takeWhile (/= ':') path
    else throw $ InvalidRepository path  -- Протокол неверный, кидаем...
    where startsWith url prefix = startswith prefix url

main :: IO ()
main = do
    result <- try $ evaluate $ extractProtocol "ss://ul@sch/proj.git"
                    :: IO (Either SomeException String)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right protocol -> putStrLn protocol
```

Here is an output:

```bash
Fault: InvalidRepository "ss://ul@sch/proj.git"
```

We are trying to extract protocol from the full path to repository and if its wrong we throw an exception which is caught by the function `try`.

## So

* Type of own exception must be an instance of type-class `Exception`.
* We throw exceptions with `throw`.
* As usually, we catch exceptions with `try`.

