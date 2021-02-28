
open NodaTime
open Serilog


module LowLevelCore =

    let pauseOneSecond =
        do Async.Sleep(1000) |> Async.RunSynchronously

    // the F# function lists all its dependencies explicitly
    let waitUntil (log: ILogger) (clock: IClock) (instant: Instant) =

        log.Information "falling asleep"
        while clock.GetCurrentInstant() < instant
            do pauseOneSecond
        log.Information "awake again"


module HighLevelServices =

    type MyService(log: ILogger, clock: IClock) =

        // the OO class does some binding privately
        let waitUntil = LowLevelCore.waitUntil log clock

        member _.SleepFiveSeconds() =
            let inFiveSeconds = clock.GetCurrentInstant().Plus(Duration.FromSeconds(5L))
            waitUntil inFiveSeconds


open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting


[<EntryPoint>]
let main args =

    let hostBuilder = (Host
        .CreateDefaultBuilder(args)
        .ConfigureServices(fun serviceCollection ->
            do serviceCollection.AddSingleton<IClock>(SystemClock.Instance) |> ignore
            do serviceCollection.AddSingleton<ILogger>(LoggerConfiguration().WriteTo.Console().CreateLogger()) |> ignore
            do serviceCollection.AddSingleton<HighLevelServices.MyService>() |> ignore
        )
    )

    let host = hostBuilder.Build()

    let scope = host.Services.CreateScope()

    let provider = scope.ServiceProvider

    let service = provider.GetRequiredService<HighLevelServices.MyService>()

    do service.SleepFiveSeconds()

    0
