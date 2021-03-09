(*---
    START of the world
  ---*)

// there will be generic things around
//
//   (some just defined by us)
//   (some pulled in from surrounding ecosystems)
//   (none of it steming from our business domain, but useful in there)
//
//   (in the real world, any database would be in here)
//   (in the real world, any public standard data models would be in here)
//
//   (to our business domain, purity and/or testability here is of no concern;
//    from our business domain's perspective, it is fair to assume that
//    everything in up here is correct)

module ExternalBasicThings =

    let pauseOneSecond() =
        do Async.Sleep(1000) |> Async.RunSynchronously

(*-----------------------------------------
    START of our domain
  -----------------------------------------*)

// we have needs
//   (and we do not care whether they are pure or easy-to-test)
//   (we assume they are safe)
open NodaTime
open Serilog

(*==========================================
    START of our pure business domain
  ==========================================*)

// one module of our business domain
//   (ideally pure and easy-to-test)
module BusinessCore =

    // any F# function lists all its dependencies explicitly
    let waitUntil (log: ILogger) (clock: IClock) (instant: Instant) =

        log.Information "falling asleep"
        while clock.GetCurrentInstant() < instant
            do ExternalBasicThings.pauseOneSecond()
        log.Information "awake again"


(*==========================================
    END of pure business domain
  ==========================================*)


// our domain boundary
//   (providing, e.g., an HTTP or gRPC server)
module BoundaryServices =

    type MyService(log: ILogger, clock: IClock) =

        // the OO class does some binding privately
        let waitUntil = BusinessCore.waitUntil log clock

        member _.SleepFiveSeconds() =
            let inFiveSeconds = clock.GetCurrentInstant().Plus(Duration.FromSeconds(5L))
            waitUntil inFiveSeconds

(*-----------------------------------------
    END of our domain - is it here? (see further below)
  -----------------------------------------*)

// if END of our domain is up here, we are giving something back to the world open for reuse
//   (namely, code that can be reused for testing in integration/staging, or for use in production)

// some more needs to get it running (when using IHost with DI)
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

let addSingletonInstance<'TService when 'TService : not struct> (serviceCollection: IServiceCollection) (instance: 'TService) =
    do serviceCollection.AddSingleton<'TService>(instance) |> ignore

// this is just the .NET top-level glue logic to get our system running
//   (bringing heavenly code down to earth always requires some unexplainable rain-making ritual)
[<EntryPoint>]
let main args =

    let injectionConfig (serviceCollection: IServiceCollection) =
        do addSingletonInstance<IClock> serviceCollection SystemClock.Instance
        do addSingletonInstance<ILogger> serviceCollection (LoggerConfiguration().WriteTo.Console().CreateLogger())
        do serviceCollection.AddSingleton<BoundaryServices.MyService>() |> ignore

    let hostBuilder = Host.CreateDefaultBuilder(args).ConfigureServices(injectionConfig)

    let host = hostBuilder.Build()

    use scope = host.Services.CreateScope()

    let provider = scope.ServiceProvider

    let service = provider.GetRequiredService<BoundaryServices.MyService>()

    do service.SleepFiveSeconds()

    async { return host.RunAsync() |> Async.AwaitTask } |> Async.RunSynchronously |> ignore

    0

(*-----------------------------------------
    END of our domain - is it here? (see further above)
  -----------------------------------------*)

// if END of our domain is down here, nothing is given back
// (so nothing we have to worry about)

(*---
    END of the world
  ---*)
