// see https://docs.microsoft.com/en-us/ef/core/

open System
open System.ComponentModel.DataAnnotations
open Microsoft.EntityFrameworkCore
open Microsoft.EntityFrameworkCore.Sqlite
open Microsoft.Data.Sqlite
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Serilog

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger() |> ignore

// this is the entity
type Person = { FirstName: string; LastName: string }

// this is the DB pendant of the entity
type PersonEntity() =

    let mutable firstName: string = null
    let mutable lastName: string = null

    member _.FirstName
        with get () = firstName
        and set (value) = firstName <- value

    member _.LastName
        with get () = lastName
        and set (value) = lastName <- value

    member PersonEntity.FromPerson(p: Person) =
        let e = new PersonEntity()
        e.FirstName <- p.FirstName
        e.LastName <- p.LastName
        e

    member this.Person() = { FirstName = this.FirstName; LastName = this.LastName }


type internal ApplicationDbContext(options: DbContextOptions<ApplicationDbContext>) =
    inherit DbContext(options)
    member _.Persons: DbSet<PersonEntity> = null

[<EntryPoint>]
let main args =

    use connection = new SqliteConnection("Filename=:memory:")

    let inMemoryDb =
        do connection.Open()
        connection

    let dbOptions = Action<IServiceProvider, DbContextOptionsBuilder>(fun _ options -> do options.UseSqlite(inMemoryDb) |> ignore)

    let injectionConfig (serviceCollection: IServiceCollection) =
        do serviceCollection.AddSingleton<ILogger>(LoggerConfiguration().WriteTo.Console().CreateLogger()) |> ignore
        do serviceCollection.AddDbContext<ApplicationDbContext>(dbOptions) |> ignore

    let hostBuilder = Host.CreateDefaultBuilder(args).ConfigureServices(injectionConfig)
    let host = hostBuilder.Build()

    use scope = host.Services.CreateScope()
    let provider = scope.ServiceProvider

    // TODO: setup schema
    // TODO: persist something
    // TODO: restore something

    0
