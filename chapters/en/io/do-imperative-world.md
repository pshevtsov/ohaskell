----
title: do: imperative world
prevChapter: /en/io/IO-a.html
nextChapter: /en/io/exceptions-handling.html
----

You got it right: imperative world. Despite that Haskell is a purely functional language, we can write imperative code when we need. Such necessity constantly arises when we work with the outside world.

As you know, imperative approach implies that the execution of instructions goes strictly in the specified order. With pure functions such approach is excessive, that's why in the case of pure function the order in which expressions are evaluated doesn't matter.

Recall our example of working with stdin and stdout:

```haskell
main :: IO ()
main = do
    putStrLn "Input your text, please:"
    lineFromUser <- getLine               
    putStrLn $ "Not bad: " ++ lineFromUser   
```

Three steps are done.

1.  print a greeting string;
2.  wait for the user input;
3.  print the final line.

Obviously, we expect that these steps would be done exactly in _that_ order. That would be odd if the final string was printed before we got the string from the user. While working with the outside world a particular order of steps is always implied. For example, when server gets request from client it first must process it, then generate the response, and only then send it to the client.

That's what the keyword `do` exists for: it binds our actions into a consecutive chain. When discussing that keyword, term "do-notation" is usually used.

## Not only the main

We can use do-notation in any function with side-effects. For example:

```haskell
obtainUserText :: String -> IO String
obtainUserText prompt = do
    putStrLn prompt  -- Print prompt to input the string
    getLine          -- Get some string from user

main :: IO ()
main = do
    firstText <- obtainUserText "Enter your text, please: "
    secondText <- obtainUserText "One more, please: "
    putStrLn $ "You said '" ++ firstText ++ "' and '" ++ secondText ++ "'"
```

Function `obtainUserText` includes two consecutive steps, that's why the keyword `do` is used. We expect that the certain prompt would be printed first and only then the action created by function `getLine` would be send to the outside world and would bring back the string which is typed by user.

## About function "return"

C language has a keyword `return` it defines the point of function's return. Haskell doesn't have such keyword, but has such function. To demonstrate its usage we consider another example:

```haskell
obtainTwoTextsFromUser :: IO String
obtainTwoTextsFromUser = do
    putStrLn "Enter your text, please: "
    firstText <- getLine
    putStrLn "One more, please: "
    secondText <- getLine
    "'" ++ firstText ++ "' and '" ++ secondText ++ "'" -- Normal string??

main :: IO ()
main = do
    twoTexts <- obtainTwoTextsFromUser
    putStrLn $ "You said " ++ twoTexts
```

Function `obtainTwoTextsFromUser` takes the responsibility to consecutively get two texts from user and return their concatenation. Unfortunately such code wouldn't compile, as function should return an action, but last instruction is absolutely not an action, it's a normal string. Here the standard function `return` would assist us.

Rewrite that function:

```haskell
obtainTwoTextsFromUser :: IO String
obtainTwoTextsFromUser = do
    putStrLn "Enter your text, please: "
    firstText <- getLine
    putStrLn "One more, please: "
    secondText <- getLine
    return $ "'" ++ firstText ++ "' and '" ++ secondText ++ "'"
```

Looks imperativish, isn't it? Do not forget that such notation has nothing to do with `return` from the C language.

Function `return` takes a value and wraps it into the action which returns that value. In our case we passed it a string, concatenated from user's texts and got an action which returns that string. Now our code successfully compiles.

By the way, to prove you that function `return` has nothing to do with C-like keyword `return`, I'll show you a small trick.


```haskell
obtainTwoTextsFromUser :: IO String
obtainTwoTextsFromUser = do
    putStrLn "Enter your text, please: "
    firstText <- getLine
    putStrLn "One more, please: "
    secondText <- getLine
    return $ "'" ++ firstText ++ "' and '" ++ secondText ++ "'"
    putStrLn "And third text, please: " -- We still going!!!!
    getLine
```

That might confuse programmers who are experienced with imperative programming, but as said above, function `return` simply wraps a value into action, which returns that value. It doesn't stop the flow of actions and if any actions follow it they simply continue the execution. In fact, in this tricky function we'll lose first two user's texts and return an action which only brings the third text.

## So

* Keyword `do` binds action into a consecutive chain.
* Function `return` wraps value into an IO-action. Do not confuse it with a keyword `return` in C-like languages.

Done. Now you know about the keyword `do`. However, you still do not know the most interesting part, Soon I'll show it to you.

