module CrossGroupTests

open Expecto
open Ch.PatchCode.SudokuSolver

let private predicate actual expected =
    expected.Level = actual.Level &&
    expected.Source |> Seq.forall (fun pair -> actual.Source |> Seq.contains pair) &&
    expected.Target |> Seq.forall (fun pair -> actual.Target |> Seq.contains pair) &&
    expected.Intersection |> Seq.forall (fun pair -> actual.Intersection |> Seq.contains pair)

let private groupsContain (expected: CrossGroup) (actuals: CrossGroup seq) : bool =
    actuals
    |> Seq.tryFind (fun actual -> predicate actual expected)
    |> Option.isSome

[<Tests>]
let tests =
    testList "Sudoku CrossGroup tests" [

        test "2 by 2 corner group" {
            for x in CrossGroup.groupsAtLevel 2 2 1 do
                printfn "l:%d i:%A s:%A t:%A" x.Level x.Intersection x.Source x.Target
            Expect.isTrue (CrossGroup.groupsAtLevel 2 2 1 |> groupsContain {
                Level = 1
                Source = seq [(0, 2); (0, 3)]
                Intersection = seq [(0, 0); (0, 1)]
                Target = seq [(1, 0); (1, 1)]
            }) "first row"
        }

        test "2 by 2 group count" {
            Expect.equal (CrossGroup.groupsAtLevel 2 2 1 |> Seq.length) 16 "all 2x2 groups"
        }

        test "3 by 3 group count" {
            Expect.equal (CrossGroup.groupsAtLevel 3 3 1 |> Seq.length) 54 "all 3x3 groups"
        }
    ]
