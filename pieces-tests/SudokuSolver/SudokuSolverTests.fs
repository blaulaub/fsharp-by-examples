module SudokuSolverTests

open Expecto
open SudokuSolver

let difficultBoard : Board =
    [|
        [| None  ; None  ; Some 9; Some 4; None  ; Some 6; Some 1; None  ; None   |];
        [| None  ; None  ; None  ; Some 5; None  ; Some 7; None  ; None  ; None   |];
        [| Some 5; None  ; None  ; None  ; Some 8; None  ; None  ; None  ; Some 3 |];
        [| Some 4; Some 5; None  ; None  ; None  ; None  ; None  ; Some 2; Some 9 |];
        [| None  ; None  ; Some 2; None  ; None  ; None  ; Some 7; None  ; None   |];
        [| Some 1; Some 6; None  ; None  ; None  ; None  ; None  ; Some 3; Some 4 |];
        [| Some 9; None  ; None  ; None  ; Some 5; None  ; None  ; None  ; Some 8 |];
        [| None  ; None  ; None  ; Some 9; None  ; Some 1; None  ; None  ; None   |];
        [| None  ; None  ; Some 1; Some 8; None  ; Some 4; Some 3; None  ; None   |]
    |]

[<Tests>]
let tests =
    testList "Sudoku solver tests" [

        let getReturnsSet row col num =
            emptyBoard() |> setNum row col num |> getNum row col = num

        test "set and get" {
            Expect.isTrue (getReturnsSet 2 3 (Some 5)) ""
        }

        test "get row 0" {
            Expect.equal (getRow 0 difficultBoard) [| None; None; Some 9; Some 4; None; Some 6; Some 1; None; None |] ""
        }

        test "get row 8" {
            Expect.equal (getRow 8 difficultBoard) [| None; None; Some 1; Some 8; None; Some 4; Some 3; None; None |] ""
        }

        test "get col 0" {
            Expect.equal (getCol 0 difficultBoard) [| None; None; Some 5; Some 4; None; Some 1; Some 9; None; None |] ""
        }

        test "get col 8" {
            Expect.equal (getCol 8 difficultBoard) [| None; None; Some 3; Some 9; None; Some 4; Some 8; None  ; None |] ""
        }

        test "get square 0 1" {
            Expect.equal (getSquare 0 1 difficultBoard) [| Some 4; None; Some 6; Some 5; None; Some 7; None; Some 8; None |] ""
        }

        test "get square 2 2" {
            Expect.equal (getSquare 2 2 difficultBoard) [| None; None; Some 8; None; None; None; Some 3; None; None |] ""
        }

        test "get missing" {
            Expect.equal (getMissing [| Some 5; Some 3; None; Some 7 |]) [1;2;4;6;8;9] ""
        }

        test "get no missing for 1..9" {
            Expect.equal (getMissing ([| 1 .. 9 |] |> Array.map (fun x -> Some x))) [] ""
        }

        test "get 1..9 missing for empty" {
            Expect.equal (getMissing ([| 1 .. 9 |] |> Array.map (fun x -> None))) [ 1 .. 9 ] ""
        }
    ]
