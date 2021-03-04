# About

This is some small example code brought up for discussing dependency injection.

Here the current
[.NET dependency injection pattern](https://docs.microsoft.com/en-us/dotnet/core/extensions/dependency-injection)
is used for the top-level OO setup.

# Creation

This project was created by running (in the solution folder):
```
dotnet new console -lang "F#" -o dependency-injection
dotnet sln add dependency-injection
```

Following dependencies have been added for the example functionality (wait and log):
```
dotnet add dependency-injection package Serilog
dotnet add dependency-injection package NodaTime
dotnet add dependency-injection package Serilog.Sinks.Console
```

Following dependencies have been added for the dependency injection:
```
dotnet add dependency-injection package Microsoft.Extensions.DependencyInjection
dotnet add dependency-injection package Microsoft.Extensions.Hosting
```

For execution of the example, simply run:
```
dotnet run -p dependency-injection
```
