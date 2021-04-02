module RuleGroupTests

open Expecto
open Ch.PatchCode.SudokuSolver

let private groupsContain expected actuals =
    actuals
    |> Seq.tryFind (fun group ->
        expected
        |> Seq.forall (fun pair -> group |> Seq.contains pair)
        )
    |> Option.isSome


[<Tests>]
let tests =
    testList "Sudoku RuleGroup tests" [

        test "2 by 2 corner groups" {
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(0, 0); (0, 1); (0, 2); (0, 3)]) "first row"
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(3, 0); (3, 1); (3, 2); (3, 3)]) "last row"
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(0, 0); (1, 0); (2, 0); (3, 0)]) "first column"
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(0, 3); (1, 3); (2, 3); (3, 3)]) "last column"
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(0, 0); (0, 1); (1, 0); (1, 1)]) "first block"
            Expect.isTrue (RuleGroup.groups 2 2 |> groupsContain [(2, 2); (2, 3); (3, 2); (3, 3)]) "last block"
        }

        test "2 by 2 group count" {
            Expect.equal (RuleGroup.groups 2 2 |> Seq.length) (3*2*2) "2 by 2"
        }

        test "3 by 3 group count" {
            Expect.equal (RuleGroup.groups 3 3 |> Seq.length) (3*3*3) "3 by 3"
        }

        test "4 by 4 group count" {
            Expect.equal (RuleGroup.groups 4 4 |> Seq.length) (3*4*4) "4 by 4"
        }
    ]
