module SudokuSolverTests

open Expecto
open SudokuSolver

[<Tests>]
let tests =
    testList "blub" [

        let getReturnsSet row col num =
            emptyBoard() |> setNum row col num |> getNum row col = num

        test "blub" {
            Expect.isTrue (getReturnsSet 2 3 (Some 5)) "blub"
        }
    ]
