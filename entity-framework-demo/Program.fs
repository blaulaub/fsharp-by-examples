// see https://docs.microsoft.com/en-us/ef/core/

open System
open System.ComponentModel.DataAnnotations
open Microsoft.EntityFrameworkCore
open Microsoft.Data.Sqlite
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Serilog

// this is the F# entity
type Person = {
    ID: int option
    FirstName: string;
    LastName: string
}

// this is the corresponding DB entity
type PersonEntity() =

    // afaik, entity framework prefers backing fields
    let mutable id: int = 0
    let mutable firstName: string = null
    let mutable lastName: string = null

    [<Key>]
    member _.ID
        with get () = id
        and set (value) = id <- value

    member _.FirstName
        with get () = firstName
        and set (value) = firstName <- value

    member _.LastName
        with get () = lastName
        and set (value) = lastName <- value

    static member FromPerson(p: Person) =
        let e = new PersonEntity()
        match p.ID with Some id -> e.ID <- id | None -> ()
        e.FirstName <- p.FirstName
        e.LastName <- p.LastName
        e

    member this.ToPerson() = {
        ID = Some this.ID;
        FirstName = this.FirstName;
        LastName = this.LastName
        }

// This is the DB context (corresponding to the schema)
type ApplicationDbContext(options: DbContextOptions<ApplicationDbContext>) =
    inherit DbContext(options)

    [<DefaultValue>] val mutable persons: DbSet<PersonEntity>
    member this.Persons with get () = this.persons and set (value) = this.persons <- value

    override this.OnModelCreating modelBuilder =
        do
            let personBuilder = modelBuilder.Entity<PersonEntity>()
            do personBuilder.Property(fun p -> p.FirstName) |> ignore
            do personBuilder.Property(fun p -> p.LastName) |> ignore
            do personBuilder.ToTable("Persons") |> ignore

[<EntryPoint>]
let main args =

    use inMemoryDb =
        let connection = new SqliteConnection("DataSource=file::memory:?cache=shared")
        do connection.Open()
        connection

    let dbOptions = Action<IServiceProvider, DbContextOptionsBuilder>(fun _ options ->
        do options.UseSqlite(inMemoryDb) |> ignore
        // note: schema migration would also belong here
        )

    let injectionConfig (serviceCollection: IServiceCollection) =
        do serviceCollection.AddSingleton<ILogger>(LoggerConfiguration().WriteTo.Console().CreateLogger()) |> ignore
        do serviceCollection.AddDbContext<ApplicationDbContext>(dbOptions, ServiceLifetime.Transient) |> ignore

    let hostBuilder = Host.CreateDefaultBuilder(args).ConfigureServices(injectionConfig)
    let host = hostBuilder.Build()

    use scope = host.Services.CreateScope()
    let provider = scope.ServiceProvider

    let logger = provider.GetRequiredService<ILogger>()

    // for in-memory sqlite to with transient (threadsafe) contexts, keep one (unused) connection alive
    do
        let context = provider.GetRequiredService<ApplicationDbContext>()
        do context.Database.OpenConnection() |> ignore
        do context.Database.EnsureCreated() |> ignore

    // persist something
    do
        let person ={ ID = None; FirstName = "Michael"; LastName = "Jackson"}
        logger.Information $"unpersisted: {person.FirstName} {person.LastName}"
        use context = provider.GetRequiredService<ApplicationDbContext>()
        person |> PersonEntity.FromPerson |> context.Persons.Add |> ignore
        context.SaveChanges() |> ignore

    // retrieve something
    do
        let person =
            async {
                use context = provider.GetRequiredService<ApplicationDbContext>()
                let! p = context.Persons.FirstAsync() |> Async.AwaitTask
                return p.ToPerson()
            } |> Async.RunSynchronously
        logger.Information $"unpersisted: {person.FirstName} {person.LastName} with ID {person.ID}"

    async { return host.RunAsync() |> Async.AwaitTask } |> Async.RunSynchronously |> ignore
    0
