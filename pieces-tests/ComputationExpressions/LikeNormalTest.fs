namespace ChPatchcode.FSharpByExamples.ComputationExpressions

open Expecto

module LikeNormalTest =

    [<Tests>]
    let tests =
        testList "A test group" [

            test "one test" {
                Expect.equal (2+2) 4 "2+2"
            }

        ]
