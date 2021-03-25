module SudokuSolverTests

open Expecto

/// Sample data for a difficult Sudoku
let difficultSudoku =
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

// Sample data for an easy Sudoku
let easySudoku =
    [|
        [|Some 5; Some 4; None  ; None  ; Some 3; None  ; None  ; Some 9; Some 2|];
        [|Some 2; None  ; Some 3; Some 7; None  ; Some 5; Some 1; None  ; Some 6|];
        [|None  ; Some 9; None  ; Some 4; None  ; Some 6; None  ; Some 5; None  |];
        [|None  ; Some 2; Some 9; None  ; None  ; None  ; Some 5; Some 1; None  |];
        [|Some 1; None  ; None  ; None  ; None  ; None  ; None  ; None  ; Some 9|];
        [|None  ; Some 7; Some 5; None  ; None  ; None  ; Some 3; Some 6; None  |];
        [|None  ; Some 1; None  ; Some 2; None  ; Some 9; None  ; Some 3; None  |];
        [|Some 9; None  ; Some 7; Some 8; None  ; Some 3; Some 4; None  ; Some 1|];
        [|Some 3; Some 5; None  ; None  ; Some 4; None  ; None  ; Some 7; Some 8|]
    |]

[<Tests>]
let tests =
    testList "Sudoku solver tests" [

        test "verify countPerValue" {
            Expect.equal [| |] (SudokuSolver.countPerValue [| |]) "empty of zero"
            Expect.equal [| 0 |] (SudokuSolver.countPerValue [| [] |]) "empty of one"
            Expect.equal [| 1 |] (SudokuSolver.countPerValue [| [1] |]) "one of one"
            Expect.equal [| 1; 1 |] (SudokuSolver.countPerValue [| [1]; [2] |]) "one of two each"
            Expect.equal [| 2; 0 |] (SudokuSolver.countPerValue [| [1]; [1] |]) "first of two twice"
            Expect.equal [| 0; 2 |] (SudokuSolver.countPerValue [| [2]; [2] |]) "second of two twice"
            Expect.equal [| 2; 2; 2 |] (SudokuSolver.countPerValue [| [1;2]; [2;3]; [3;1] |]) "circular with two from three"
            Expect.equal [| 1; 2; 3 |] (SudokuSolver.countPerValue [| [1;2;3]; [2;3]; [3] |]) "descending with three"
        }

        test "verify toOrderedPresence" {
            let subSorted = Array.map (List.sort)
            Expect.equal (subSorted [| |]) (SudokuSolver.toOrderedPresence [| |] |> subSorted) "empty of zero"
            Expect.equal (subSorted [| [] |]) (SudokuSolver.toOrderedPresence [| [] |] |> subSorted) "empty of one"
            Expect.equal (subSorted [| [0] |]) (SudokuSolver.toOrderedPresence [| [1] |] |> subSorted) "one of one"
            Expect.equal (subSorted [| [0]; [1] |]) (SudokuSolver.toOrderedPresence [| [1]; [2] |] |> subSorted) "one of two each"
            Expect.equal (subSorted [| [1;0]; [] |]) (SudokuSolver.toOrderedPresence [| [1]; [1] |] |> subSorted) "first of two twice"
            Expect.equal (subSorted [| []; [1;0] |]) (SudokuSolver.toOrderedPresence [| [2]; [2] |] |> subSorted) "second of two twice"
            Expect.equal (subSorted [| [2;0]; [1;0]; [2;1] |]) (SudokuSolver.toOrderedPresence [| [1;2]; [2;3]; [3;1] |] |> subSorted) "circular with two from three"
            Expect.equal (subSorted [| [0]; [1;0]; [2;1;0] |]) (SudokuSolver.toOrderedPresence [| [1;2;3]; [2;3]; [3] |] |> subSorted) "descending with three"
        }

        test "try solve some board" {
            let initial = easySudoku
            let final = SudokuSolver.solve initial
            SudokuSolver.toString final |> printfn "%s"
            Expect.equal final initial "no progress (yet)"
        }
    ]
