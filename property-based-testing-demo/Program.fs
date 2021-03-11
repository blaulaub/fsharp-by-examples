// see https://www.youtube.com/watch?v=IYzDFHx6QPY

open Expecto

// some extremly complicated (and possibly bogus) implementation
let plus x y =
    match (x, y) with
    | (14, 7) -> 21
    | _ -> 4


// tests for the extremly complicated implementation
module Tests =

    /// <summary>abstraction of classical assertion testing</summary>
    /// <remarks>Some given input (could be a corner case) is feed
    /// into the testee, and the outcome is compared to an expected
    /// result.</remarks>
    let assertionTest input testee expected =
        let actual = input |> testee
        Expect.equal actual expected "actual result does not match expected"

    /// <summary>Testsuite of classical assertion tests.</summary>
    [<Tests>]
    let assertionTests =
        testList "AssertionTests" [
            test "1 plus 3 gives 4"   { assertionTest (1, 3)  (fun (x, y) -> plus x y) (4)  }
            test "14 plus 7 gives 21" { assertionTest (14, 7) (fun (x, y) -> plus x y) (21) }
            // and we can write much much more...
        ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
