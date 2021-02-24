This project contains the unit tests and the test runner for [pieces](../pieces).

[Expecto](https://github.com/haf/expecto) is used as test framework.

This project was created by running following `dotnet` commands in the solution folder:
```
dotnet new console -lang "F#" -o pieces-tests
dotnet sln add pieces-tests
dotnet add pieces-tests reference pieces
dotnet add pieces-tests package Expecto
```

Tests can be run from the solution folder via `dotnet`
```
dotnet run pieces-tests
```
*or* using the test runner executable (the precise binary path may vary), e.g.
```
dotnet build
./pieces-tests/bin/Debug/net5.0/pieces-tests
```
