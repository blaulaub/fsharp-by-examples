# Learning about

For understanding the code in here, some helpful resources on computation expressions are:

- the [Microsoft "Computation Expressions" documentation](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)
- the [corresponding fun-and-profit series](https://fsharpforfunandprofit.com/series/computation-expressions/)

The fun-and-profit series is more helpful, the Microsoft documentation is more formal and ultimatly more complete.

# Trivial case: like normal

Computation expressions add extra semantics (here: side-effects) to otherwise pure expressions.
The trivial case would be no extra semantics (no extra side effects), this is what [LikeNormal.fs](LikeNormal.fs)
tries to implement.
