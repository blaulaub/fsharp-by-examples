namespace ChPatchcode.FSharpByExamples.ComputationExpressions

open Expecto
open Maybe

module MaybeTest =

    [<Tests>]
    let tests =
        testList "Workflow Maybe" [

            (*
                Fashioned after https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
            *)
            test "Bind and Return" {

                let divideBy bottom top =
                    if bottom = 0
                    then None
                    else Some(top/bottom)

                let divideThreeTimes dividend divisor1 divisor2 divisor3 =
                    maybe {
                        let! a = Some dividend
                        let! b = a |> divideBy divisor1
                        let! c = b |> divideBy divisor2
                        let! d = c |> divideBy divisor3
                        return d
                    }

                Expect.equal (divideThreeTimes 12 3 2 1) (Some 2) "good"
                Expect.equal (divideThreeTimes 12 3 0 1) (None) "bad"
            }
        ]
