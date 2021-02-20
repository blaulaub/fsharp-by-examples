# Learning about

For understanding the basics of the code in here, some helpful resources on computation expressions are:

- the Microsoft "Computation Expressions" [documentation](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)
- the corresponding Fun & Profit ["Computation Expressions" series](https://fsharpforfunandprofit.com/series/computation-expressions/)

The Fun & Profit series is more helpful, the Microsoft documentation is more formal and ultimatly more complete.

# Additional learn-about material

Regarding code free from side effects versus similar code with side effects, I'd also recommend to have a glance at
the Fun & Profit ["Map and Bind and Apply, Oh my!" series](https://fsharpforfunandprofit.com/series/map-and-bind-and-apply-oh-my/).
In there, the world with side-effects is labeled as "elevated world", the world without side-effects as "normal world".

# Trivial case: like normal

Computation expressions add extra semantics (here: side-effects) to otherwise pure expressions.
The trivial case would be no extra semantics (no extra side effects), this is what [LikeNormal.fs](LikeNormal.fs)
tries to implement.
