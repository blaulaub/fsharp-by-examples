module EliminationTests

open Expecto
open Ch.PatchCode.SudokuSolver

[<Tests>]
let tests =
    testList "Sudoku Elimination tests" [

        test "remove first of many" {
            let before =
                [|
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |]
                |]

            let after =
                {Row = 0; Col = 0; Value = 0}
                |> Elimination.eliminate 2 2 before

            let expected =
                [|
                    [| [1..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |]
                |]

            Expect.equal after expected "only in field 0 0 should 0 be missing"
        }

        test "remove last of many" {
            let before =
                [|
                    [| [0..1]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |]
                |]

            let after =
                {Row = 0; Col = 0; Value = 1}
                |> Elimination.eliminate 2 2 before

            let expected =
                [|
                    [| [0..0]; [1..3]; [1..3]; [1..3] |];
                    [| [1..3]; [1..3]; [0..3]; [0..3] |];
                    [| [1..3]; [0..3]; [0..3]; [0..3] |];
                    [| [1..3]; [0..3]; [0..3]; [0..3] |]
                |]

            Expect.equal after expected "only in field 0 0 should 0 be missing"
        }

        test "remove recursively" {
            let before =
                [|
                    [| [0..1]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..3]; [0..3]; [0..3]; [0..3] |];
                    [| [0..1]; [0..3]; [0..3]; [0..3] |]
                |]

            let after =
                {Row = 0; Col = 0; Value = 1}
                |> Elimination.eliminate 2 2 before

            let expected =
                [|
                    [| [0]   ; [1..3] ; [1..3] ; [1..3]  |];
                    [| [2..3]; [1..3] ; [0..3] ; [0..3]  |];
                    [| [2..3]; [0;2;3]; [0..3] ; [0..3]  |];
                    [| [1]   ; [0;2;3]; [0;2;3]; [0;2;3] |]
                |]

            Expect.equal after expected "only in field 0 0 should 0 be missing"
        }
    ]
