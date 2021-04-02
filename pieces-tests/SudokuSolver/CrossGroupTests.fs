module CrossGroupTests

open Expecto
open Ch.PatchCode.SudokuSolver

let predicate actual expected =
    expected.Source |> Seq.forall (fun pair -> actual.Source |> Seq.contains pair) &&
    expected.Target |> Seq.forall (fun pair -> actual.Target |> Seq.contains pair) &&
    expected.Intersection |> Seq.forall (fun pair -> actual.Intersection |> Seq.contains pair)

let private groupsContain (expected: SingularCrossGroup) (actuals: SingularCrossGroup seq) : bool =
    actuals
    |> Seq.tryFind (fun actual -> predicate actual expected)
    |> Option.isSome

[<Tests>]
let tests =
    testList "Sudoku CrossGroup tests" [

        test "2 by 2 corner group" {
            Expect.isTrue (CrossGroup.groups 2 2 |> groupsContain {
                Source = seq [(0, 2); (0, 3)]
                Intersection = seq [(0, 0); (0, 1)]
                Target = seq [(1, 0); (1, 1)]
            }) "first row"
        }

        test "2 by 2 group count" {
            Expect.equal (CrossGroup.groups 2 2 |> Seq.length) 16 "first row"
        }
    ]
