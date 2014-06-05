Where we can use it
-------------------

Almost anywhere. Haskell is a general purpose language, so it's applicable to different segments of software. Client-server development, command-line utilities, desktop apps with GUI, mobile apps and even web-development. Moreover, Haskell not just *can* be used for such a development, but used *efficiently*.

### Why

#### High-level syntax

Code written in Haskell can be compared with code written in script languages. So we have a code that simple to read and, as a result, simple to maintain. Of course, we *can* write an awful trash in Haskell, but trash can be written in any programming language.

#### Automatic memory management

Now is 2014, so, with all due respect to the good old C, it's a stone age if we have to think about how/where/when to allocate N bytes of memory. Haskell freeing the developer from the stone age and takes the memory allocation/deallocation upon itself.

#### Strict static typing 

Due fact that type errors will be catched at the compile time, we'll get a more reliable code. Compile error is always better then runtime error, isn't it?

#### Compilable

Haskell code compiles into simple, canonical executable file. So we get a fast software which runs without interpreters or additional runtime environments.

#### Ready solutions

We have many, many packages for a Haskell. No, not all of them are perfect, but Hackage contains a big number of well-tested, reliable and useful packages, so we won't to reinvent the wheel.

#### Parallel world

We won't to study parallel and concurrent programing in this book, because there's one [excellent book](http://chimera.labs.oreilly.com/books/1230000000929) about this. Just say that due pure functional nature of a Haskell we can write parallel code [much easier](https://www.fpcomplete.com/blog/2012/04/the-downfall-of-imperative-programming) than in languages like C++ or Java. 

### Main restriction

Yes, there's one area not for a Haskell. It's about a low-level development that close to a hardware. Where the speed of code execution is crucial, where you want to manage memory manually because of its small volume, where the size of the executable file is limited to tens of kilobytes - Haskell doesn't fit there. So if you need to write some high-performance driver or part of the operating system kernel - I think you need to look at other languages.

And now let's begin our studying.

