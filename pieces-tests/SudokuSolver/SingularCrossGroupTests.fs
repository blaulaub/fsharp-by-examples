module SingularCrossGroupTests

open Expecto
open Ch.PatchCode.SudokuSolver

let private predicate actual expected =
    expected.Source |> Seq.forall (fun pair -> actual.Source |> Seq.contains pair) &&
    expected.Target |> Seq.forall (fun pair -> actual.Target |> Seq.contains pair) &&
    expected.Intersection |> Seq.forall (fun pair -> actual.Intersection |> Seq.contains pair)

let private groupsContain (expected: SingularCrossGroup) (actuals: SingularCrossGroup seq) : bool =
    actuals
    |> Seq.tryFind (fun actual -> predicate actual expected)
    |> Option.isSome

[<Tests>]
let tests =
    testList "Sudoku SingularCrossGroup tests" [

        test "2 by 2 corner group" {
            Expect.isTrue (SingularCrossGroup.groups 2 2 |> groupsContain {
                Source = seq [(0, 2); (0, 3)]
                Intersection = seq [(0, 0); (0, 1)]
                Target = seq [(1, 0); (1, 1)]
            }) "first row"
        }

        test "2 by 2 group count" {
            Expect.equal (SingularCrossGroup.groups 2 2 |> Seq.length) 16 "all 2x2 groups"
        }

        test "3 by 3 group count" {
            Expect.equal (SingularCrossGroup.groups 3 3 |> Seq.length) 54 "all 3x3 groups"
        }

        test "4 by 4 group count" {
            Expect.equal (SingularCrossGroup.groups 4 4 |> Seq.length) (2 * 4*4*4) "all 4x4 groups"
        }
    ]
