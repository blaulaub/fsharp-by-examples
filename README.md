# Brief: .NET CLI Commands

Create a solution with folder, like this one named  `fsharp-by-examples`:

```
dotnet new sln -o fsharp-by-examples
```

Add a non-executable class library in the current folder, like the one named [pieces](pieces):

```
dotnet new classlib -lang "F#" -o pieces
dotnet sln add pieces
```
