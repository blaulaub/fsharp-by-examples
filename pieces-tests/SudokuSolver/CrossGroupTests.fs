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

        test "4 by 4 group count" {
            Expect.equal (CrossGroup.groupsAtLevel 4 4 1 |> Seq.length) (2 * 4*4*4) "all 4x4 groups"
        }

        test "4 by 4 group count at level 2" {
            let twoOutOfFour = 6
            // note: wrong on purpose, the underlying impl is not complete yet
            Expect.equal (CrossGroup.groupsAtLevel 4 4 2 |> Seq.length) (2 * 4*4*twoOutOfFour) "all 4x4 groups at level 2"
        }

        test "3 by 4 level 1 corner group" {
            Expect.isTrue (CrossGroup.groupsAtLevel 3 4 1 |> groupsContain {
                Level = 1
                Intersection = seq { for col in 0..2 -> (0, col)}
                Source = seq { for col in 3..11 -> (0, col)}
                Target = seq { for row in 1..3 do for col in 0..2 -> (row, col)}
            }) "first row"
        }

        test "4 by 3 level 1 corner group" {
            Expect.isTrue (CrossGroup.groupsAtLevel 4 3 1 |> groupsContain {
                Level = 1
                Intersection = seq { for col in 0..3 -> (0, col)}
                Source = seq { for col in 4..11 -> (0, col)}
                Target = seq { for row in 1..2 do for col in 0..3 -> (row, col)}
            }) "first row"
        }

        test "4 by 4 level 1 corner group" {
            for x in CrossGroup.groupsAtLevel 4 4 1 do
                printfn "l:%d i:%A s:%A t:%A" x.Level x.Intersection x.Source x.Target
            Expect.isTrue (CrossGroup.groupsAtLevel 4 4 1 |> groupsContain {
                Level = 1
                Intersection = seq { for col in 0..3 -> (0, col)}
                Source = seq { for col in 4..15 -> (0, col)}
                Target = seq { for row in 1..3 do for col in 0..3 -> (row, col)}
            }) "first row"
        }

    ]
