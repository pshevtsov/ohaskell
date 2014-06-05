Functions and operators
-----------------------

To avoid confusion let's clarify the terminology. You'll see a words "function" and "operator" many times in this book. Operator is a function with name that contains only symbols (without letters). For example, operator of concatenation of two lists:

```haskell
let fullAddress = "127.0.0.1" ++ ":80"
```

This operator contains two plus signs. Yes, we *could* name it a *function* and even write it in the non-operator form:

```haskell
let fullAddress = (++) "127.0.0.1" ":80"
```

But such a function commonly called an operator. Moreover, almost all operators are intended for the infix form. Compare this form:

```haskell
1 + 2
```

with this form:

```haskell
(+) 1 2
```

First variant is a simpler, isn't it? Of course, there's a functions for the infix form, for example:

```haskell
if ".com" `isSuffixOf` "google.com"
```

But name of such a functions are not symbol-only, so it's a functions, not an operator.

So, we *can* name an operator as a function, but not vice versa. Better choice is to name a function as a "function", and operator as an "operator". 

