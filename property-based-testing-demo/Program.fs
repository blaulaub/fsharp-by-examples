// this F# code file contains a drafted example for trying to understand
// the first half of https://www.youtube.com/watch?v=IYzDFHx6QPY


// the testing framework used here (see https://github.com/haf/expecto)
open Expecto


// === below is what we want to test ===


/// <summary>
/// Some extremly complicated (and maybe bogus) code.
/// </summary>
let plus x y =
    match (x, y) with
    // special cases considered by the enterprise programmer from hell
    | (14, 7) -> 21
    | (1, 3) -> 4
    // and anyway a correct implementation
    | _ -> x + y


// === below are the tests ===


/// <summary>
/// Tests for the extremly complicated implementation of <see cref="plus"/>.
/// </summary>
module Tests =


    // === Testing as we know it: Assert against expected entities or values ===


    /// <summary>
    /// Abstraction of classical assertion testing (as we know it).
    /// </summary>
    /// <remarks>Some given input (could be a corner case) is feed
    /// into the testee, and the outcome is compared to an expected
    /// result.</remarks>
    let assertionTest input testee expected =
        let actual = input |> testee
        Expect.equal actual expected "actual result does not match expected"

    /// <summary>Testsuite of classical assertion tests.</summary>
    [<Tests>]
    let assertionTests =

        // test setup

        let testee = fun (x, y) -> plus x y

        // actual tests

        testList "AssertionTests" [
            test "1 plus 3 gives 4"   { assertionTest (1, 3)  testee (4)  }
            test "14 plus 7 gives 21" { assertionTest (14, 7) testee (21) }
            // and we can write much much more...
        ]


    // === Testing as presented by Scott Wlaschin: Assert against expected functionality ===
    // (see https://www.youtube.com/watch?v=IYzDFHx6QPY)


    /// <summary>
    /// Abstraction of property testing (as presented by Scott Wlaschin).
    /// </summary>
    /// <remarks>Some inputs (could be corner cases, could be a random
    /// stream) is feed into the testee, and some property based on each
    /// actual input and output is checked.</remarks>
    let propertyTest testee property inputs =
        for input in inputs do
            property testee input

    [<Tests>]
    let propertyTests =

        // test setup

        let testee = fun (x, y) -> plus x y

        let randomInput =
            let rnd = new System.Random()
            List.init 100 (fun _ -> (rnd.Next(), rnd.Next()))

        let isCommutative testee (x, y) =
            Expect.equal (testee (x, y)) (testee (y, x)) $"not commutative for {x} {y}"

        let isAssociative testee (x, y) =
            Expect.equal (testee (testee (x, 2), y)) (testee (x, (testee (y, 2)))) $"not associative for {x} {y}"

        let hasNeutralElementZero testee (x, y) =
            Expect.equal (testee (x, 0)) (x) $"zero is not a neutral element for {x}"
            Expect.equal (testee (0, y)) (y) $"zero is not a neutral element for {y}"

        // actual tests

        testList "PropertyTests" [
            test "is commutative"           { propertyTest testee isCommutative         randomInput }
            test "is associative"           { propertyTest testee isAssociative         randomInput }
            test "has neutral element zero" { propertyTest testee hasNeutralElementZero randomInput }
        ]


// === below is the test runner ===


[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
