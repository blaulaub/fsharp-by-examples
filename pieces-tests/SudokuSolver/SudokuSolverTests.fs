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

let easyBoard : Board =
    [|
        [|Some 5; Some 4; None; None; Some 3; None; None; Some 9; Some 2|];
        [|Some 2; None; Some 3; Some 7; None; Some 5; Some 1; None; Some 6|];
        [|None; Some 9; None; Some 4; None; Some 6; None; Some 5; None|];
        [|None; Some 2; Some 9; None; None; None; Some 5; Some 1; None|];
        [|Some 1; None; None; None; None; None; None; None; Some 9|];
        [|None; Some 7; Some 5; None; None; None; Some 3; Some 6; None|];
        [|None; Some 1; None; Some 2; None; Some 9; None; Some 3; None|];
        [|Some 9; None; Some 7; Some 8; None; Some 3; Some 4; None; Some 1|];
        [|Some 3; Some 5; None; None; Some 4; None; None; Some 7; Some 8|]
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
            Expect.equal (getMissingForList [| Some 5; Some 3; None; Some 7 |] [ 1.. 9 ]) [1;2;4;6;8;9] ""
        }

        test "get no missing for 1..9" {
            Expect.equal (getMissingForList ([| 1 .. 9 |] |> Array.map (fun x -> Some x)) [ 1.. 9 ]) [] ""
        }

        test "get 1..9 missing for empty" {
            Expect.equal (getMissingForList ([| 1 .. 9 |] |> Array.map (fun x -> None)) [ 1.. 9 ]) [ 1 .. 9 ] ""
        }

        test "get missing 0 0 for difficultBoard" {
            Expect.equal (getMissingFromBoard 0 0 difficultBoard) [ 2;3;7;8 ] ""
        }

        test "get missing 1 4 for difficultBoard" {
            Expect.equal (getMissingFromBoard 1 4 difficultBoard) [ 1;2;3;9 ] ""
        }

        test "get missing 7 7 for difficultBoard" {
            Expect.equal (getMissingFromBoard 7 7 difficultBoard) [ 4;5;6;7 ] ""
        }

        test "none obviously missing on difficult board" {
            Expect.isTrue (obviouslyMissingOn difficultBoard |> isEmpty) "dummy test"
        }

        test "estimate obviously missing on easy board" {
            let missings = obviouslyMissingOn easyBoard
            printfn "%A" missings
            Expect.isFalse (missings |> isEmpty) "dummy test"
        }
    ]
