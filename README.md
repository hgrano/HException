# HException
Heterogeneously typed error handling for Haskell

## Motivation
Exception handling is one of the most important parts of writing any program. A good option for error handling in
Haskell is the `ExceptT` type from the `transformers` package. One of the problems with using this solution is the limitation of the exception type to a single type. In reality we will want to signal different exceptions in
different parts of our code. One solution to the problem is to create "wrapper" types, as shown in this
[blog post](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html). A (bad) alternative is to
just have a single large sum type for all exceptions in our program and all functions may signal an exception of this
type. HException provides a strongly-typed and composable solution in which exceptions are "heterogeneously" typed,
meaning that a function may signal any one of a range of distinct exception types. These functions and exception types
can then be composed together. A detailed example can be found under
[here](https://github.com/hgrano/HException/blob/master/examples/Animals.hs). Documentation is available
[here](https://htmlpreview.github.io/?https://github.com/hgrano/HException/blob/master/doc/index.html). Release on
Hackage is planned soon.
