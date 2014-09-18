----
title: Types at a glance
prevChapter: /en/about-user-types/index.html
nextChapter: /en/about-user-types/about-value-constructors.html
----

In the real projects we definitely need our own types. Let's talk about them.

First of all, there are new keywords. The word `data` is used to create a new type. The word `class` is used to define the type class. And the word `instance` is used to declare the class instance.

## Our first type

This is type for IP-address:
 
```haskell
data IPAddress = IPAddress String
```

Done. It's a very simple type. In fact, value of this type will be just a string with a label `IPAddress`. Label is an explanatory identifier. Many types don't have labels, so you can initialize values of such a types directly. For example, if we have a function that takes a value of type `Int`, then when we'll call it, we'll can write just:

```haskell
show 6
```
But if this function will be applied to value of our type `IPAddress`, we must specify it explicitly:

```haskell
show $ IPAddress "127.0.0.1"
```

Expression `IPAddress "127.0.0.1"` will create a value of type `IPAddress` with a string `"127.0.0.1"` inside.

By the way, label is called a *value constructor*.

And remember: name of type (as well as name of value constructor) cannot starts with lowercase letter. So such a code:

```haskell
data ipAddress = ipAddress String
```

will be rejected by compiler.

## Type class

The problem is that our type is a very primitive. If we create a value of `IPAddress` we'll cannot even print it on the terminal. And if we'll write like this:

```haskell
main = putStrLn $ show $ IPAddress "127.0.0.1"
```

compiler will show us an error:

```bash
No instance for (Show IPAddress) arising from a use of `show'
```

And compiler is absolutely right: standard function `show` that converts its argument to a string form, doesn't have a clue about how to present a value of type `IPAddress` as a string. So that where we meet a type class.

Type class is a logical group of types. It has some features common to all these types. And these features represented as a methods declared in this class. Every type that is relevant to type class should implement methods of this class. Type class can be viewed as an abstract interface.

For example, standard class `Show` unites all the types that can be "shown" (i.e. printed in a string form). This is declaration of this type class:

```haskell
class Show a where
    show :: a -> String
```

As you see there's a single method called `show`. It takes just one argument of the type `a` and returns its string form.

You ask, what is this type `a`? Earlier I mentioned the term "polymorphic type", and `a` in this example is a polymorphic type. This mean that method `show` can be applied to a values of different types. Traditionally polymorphic types is called by a single letter, but you can call them whatever you like.

But how did `show` knows *how to* stringificate value of `IPAddress`? We understand that different types can be printed differently. Class instance is here!

## Class instance

Whereas type class `Show` is a logical group of the types that can be "shown", the instance of this class is an *explanation* of how to print a value of particular type. So if we want to print our IP-addresses we must provide an instance of the class `Show` for type `IPAddress`. Let's do it:

```haskell
instance Show IPAddress where
    show (IPAddress address) =
        if address == "127.0.0.1" then "localhost" else address
```

We've used keyword `instance`, and polymorphic type `a` was replaced by type `IPAddress`. And after that we define method `show` for owr IP-addresses.  This method take a value of type `IPAddress` that was created by expression `IPAddress address`. So if address string is equal to `"127.0.0.1"`, string `"localhost"` will be printed.

And our `main`:

```haskell
main = putStrLn $ show $ IPAddress "127.0.0.1"
```

Result:

```bash
localhost
```

It works.

## So

* If we see keyword `data` - it's a user type.
* Type class - it's an abstract interface that unites group of types by some common traits.
* If some type wants to be related to some class, it should present its own instance of this class.

