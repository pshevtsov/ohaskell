Tuples
------

Tuple is a special kind of a list. It contains an elements, but there's three main differences:

1. parentheses instead of square brackets;
2. heterogeneity;
3. type that depends on the size.

First difference is a quite obvious:

```haskell
["Denis", "Shevchenko"]  -- This is a list with two strings.
("Denis", "Shevchenko")  -- This is a tuple with two strings.
```

Second difference - a heterogeneity - is an ability to store an elements of different types:

```haskell
["Denis", 1.234]  -- Be sure, it will not be compiled...
("Denis", 1.234)  -- That's OK!
```

Third difference is a little more interesting. If we have two such lists:

```haskell
["Denis", "Vasil`evich", "Shevchenko"]
["Denis", "Shevchenko"]
```

their types are the same, `[String]`, because type of the list doesn't depend on its size.

Tuples are a quite another stuff. If we have two such tuples:

```haskell
("Denis", "Vasil`evich", "Shevchenko")
("Denis", "Shevchenko")
```

their types are widely different: type of the first one is `(String, String, String)`, but type of the second is `(String, String)`. So if we have a functions that works with two-element tuple, and we'll try to apply it to three-element tuple, compiler will reject it:

```bash
Couldn't match expected type `(String, String)'
            with actual type `([Char], [Char], [Char])'
```

Type of the tuple totally depends on its size. By the way, tuple can be an empty, i.e. without any elements. Empty tuple is used for "nothing". Soon we'll see the cases when empty tuples are useful.

### What we can do with it

The only thing we can do with a tuple is to obtain its elements. Nothing more.

In practice often use tuples with two elements. Such a tuple is also called as a *pair*. To extract element from a pair we can use standard functions `fst` and `snd`.

For example, this is a function for work with a pair that stores chess move:

```haskell
chessMove :: (String, String) -> String
chessMove pair = fst pair ++ "-" ++ snd pair

main = print $ chessMove ("e2", "e4")
```
We extracted first and second elements of the pair and made a single line from it:

```bash
e2-e4
```

But what we'll do if number of tuple's elements is greater that two? In this case `fst` and `snd` are useless. So we need another way.

### Inconvenient way

This way is inconvenient because we need to define functions-extractors. But let's do it:

```haskell
get1 (element, _, _, _) = element
get2 (_, element, _, _) = element
get3 (_, _, element, _) = element
get4 (_, _, _, element) = element
```

Assumed that we'll work with 4-elements tuple. In this case there are only four cases of extraction: first element, second, third and fourth. By the way, saying "the first element", we mean it's a *first*: number `1` in the name `get1` is a number, not an index.

So, this is owr first extractor:

```haskell
get1 (element, _, _, _) = element
```

This function applies to a tuple of four elements, and returns the first one. Note the strange underscores here. Think of this as about "something". We say: "Yes, this tuple has four elements, but we don't care about second element and about third elements and about fourth element. We just want first element."

So second function-extractor is a similar:

```haskell
get2 (_, element, _, _) = element
```

We don't care about first, third and fourth elements. Only second.

And now we write `main`:

```haskell
main = print $ get3 ("One", "Two", "Three", "Four")
```

And this is our result:

```bash
"Three"
```

### Convenient way

Why reinvent the wheel? There is `tuple` package in Hackage. Let's install it:

```bash
$ cabal install tuple
```

Now we have to add name of this package to `build-depends` parameter in `Real.cabal` file:

```haskell
    build-depends:  base ==4.6.\*, MissingH, tuple
```

And now we have to import module `Data.Tuple.Select`:

```haskell
import Data.Tuple.Select

main = print $ sel3 ("One", "Two", "Three", "Four")
```

Function `sel3` extracts third element from a tuple. Convenient and easy. By the way, `tuple` package has functions from `sel1` till `sel15`. Author of this package assumed that no one developer will create tuple with 128 elements... ;-)

So, what about safety? What if we'll try to obtain fifth element from this tuple? Let's do it:

```haskell
import Data.Tuple.Select

main = print $ sel5 ("One", "Two", "Three", "Four")
```

What we'll get in this case? Error? Exception? No. Such a code will not be compiled:

```bash
src/Main.hs:23:12:
    No instance for (Sel5 ([Char], [Char], [Char], [Char]) a0)
      arising from a use of `sel5'
```

As we remember, type of the tuple depends on its size, so all overrunning errors will be catched on compile time.

### So

* Tuple is a simple kind of list that can store values of different types.
* Type of the tuple depends on its size and types of its elements. 
* The only thing we can do with a tuple is to obtain its elements.

### Let's try

Source code from this chapter available online. 

<span><a href="https://www.fpcomplete.com/ide?title=tuples&paste=https://raw.githubusercontent.com/denisshevchenko/ohaskell-code/master/code/about-lists/tuples/Main.hs" class="fpcomplete_code" target="_blank">Open in FP IDE</a></span>
<span class="buttons_space"></span>
<span><a href="https://github.com/denisshevchenko/ohaskell-code/blob/master/code/about-lists/tuples/Main.hs" class="github_code" target="_blank">Open at GitHub</a></span>
