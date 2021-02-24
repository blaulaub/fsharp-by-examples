namespace ChPatchcode.FSharpByExamples.ComputationExpressions

open Expecto
open LikeNormal

module LikeNormalTest =

    [<Tests>]
    let tests =
        testList "Workflow LikeNormal" [

            test "Bind and Return" {
                let bindAndReturn = likeNormal {
                    // NOTE the compiler translates "let!" to LikeNormalBuilder.Bind
                    let! x = 42
                    let! y = 43
                    let! z = x + y
                    // NOTE the compiler translates "return" to LikeNormalBuilder.Return
                    return z
                }

                Expect.equal bindAndReturn 85 "42+43"
            }
        ]
