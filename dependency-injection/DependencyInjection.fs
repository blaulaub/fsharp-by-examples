
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


[<EntryPoint>]
let main args =

    // setup external dependencies
    let log = LoggerConfiguration().WriteTo.Console().CreateLogger()
    let clock = NodaTime.SystemClock.Instance

    // constructor injection
    let service = HighLevelServices.MyService(log, clock)

    // execution
    service.SleepFiveSeconds()
    0
