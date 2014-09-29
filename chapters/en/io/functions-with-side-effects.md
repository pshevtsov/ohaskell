----
title: Functions With Side Effects
prevChapter: /en/io/index.html
nextChapter: /en/io/IO-a.html
----

You are already familiar with pure functions. Time to talk about functions which have side effects.

## Pure vs Impure

As you remember, pure functions have no side effects, that makes them a reflection of the mathematical concept "function": the input unambiguously defines the output. That's really great, but our application must interact with the outside world and pure functions do not look like good ambassadors to the outside world.

Consider a function that reads text from a file: it takes a string with a path to that file and returns a string with the contents of the file:

```haskell
readMyFile :: String -> String
readMyFile path =
    -- open the corresponding file
    -- read it and return the string with content...
````

But such function can't be pure. Can we guarantee that the output stays the same if we apply it twice to the string with the path to the same file? Apparently no, between the first and second application the file could be changed triply. In that case the mathematical nature of such a function would fall in pieces: was applied to the same argument twice, the results on the output are different. That's why interaction with the outside world is the land of the functions with side-effects.

## Action vs Inaction

Pure function is the world of inactivity. The world of calmness and silence. As you already know, such a function is a set of expressions which are evaluated in particular order and result into some last, final value. Then the compiler substitutes the function call with that value.

In contrast, functions with side effects are the world of action. The world in which everything changes. That's why we need actions when we work with input and output. Action is that thing which touches the outside world and often affects it. Want to read a file? Welcome to the outside world. Want to send UDP-datagram? Go to the outside world. Need to read a string typed by a user from the keyboard. The outside world is waiting for you.
