At the time of writing,
[Chapter 7, Exercise Group 3](https://book.purescript.org/chapter7.html#exercises-2)
does a trial by fire by having the reader jump straight into creating a
`Traversable` instance for a a binary Tree, without having shown:

1. a full `Traversable` instance for any type
    _(the `List` example does not define a `sequence` instance)_

2. a ground-up example of any arbitrary data type implementing the necessary
    superclass constraints (`Foldable`, `Functor`)
