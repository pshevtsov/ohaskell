----
title: IO a
prevChapter: /en/io/functions-with-side-effects.html
nextChapter: /en/io/do-imperative-world.html
----

In order to work with outside world we need actions. And an action is a value of type `IO a`, where `IO` is a standard action type and `a` is a polymorphic type of value which the action returns. As you can see, `IO` is a type constructor. That's why the type of action which returns a string is `IO String`.

From the logical point of view, action is our ambassador, when we ask he goes to the outside world, does some work and brings us something interesting from the outside world. However, he might come back empty-handed.

## Standard Input and Output

Let us start with standard channels stdout and stdin. Let's output a string to the screen:

```haskell
main = putStrLn "Hi Haskeller!"
```

Take a look at the type of function `putStrLn`:

```haskell
putStrLn :: String -> IO ()
```

We face `IO`, and therefore, that function has a side effect. We have a string on input and an action on output. Let's take a closer look:

```haskell
IO ()
```

Lonely parentheses are telling us about an empty tuple. Therefore, we have an action, which does its job in the outside world and brings us nothing. When we send this action to the outside world, we tell it, "Just go and print that string on the computer's screen". But as the designers of Haskell agreed that action must return something -- let it return an empty tuple. It's like a `void`-function in the C language: you might say that it returns nothing, but you also might say that it returns `void`.

Now let's take a look at the type of function `getLine`, which gets the string from the standard input:

```haskell
getLine :: IO String
```

Here we have an opposite situation. That function takes nothing from us as there is nothing we could give it, so the action is created but that function goes to the outside world empty-handed. We tell it, "Go, get us the string from the standard input".

## Defining main

Finally, we can take a look at the type of the main function of the application:

```haskell
main :: IO ()
```

Function `main` also performs an action -- the whole work of our application. It returns nothing to the application, after all when its execution is finished everything ends. Of course, all actions in our application are doing nothing, having sweet dreams until the action of function `main` is fired.

We haven't yet defined that function because we just found out about `IO`. However, if we want to keep things in order, we have to do it now.

## Teamwork

Here it is:

```haskell
main :: IO ()
main = do
    putStrLn "Input your text, please:"
    lineFromUser <- getLine
    putStrLn $ "Not bad: " ++ lineFromUser
```

It's perfectly clear, except two new things.

Firstly, the back-arrow `<-`. Take a look:

```haskell
lineFromUser <- getLine
```

That's a bind. We tell to action which was created by function `getLine`: "Go, get us the line from the user and bind it to its identifier `lineFromUser`, so that we can read that line".

Secondly, we met a new keyword `do`. That deserves a special discussion which you'll find in the next chapter.

