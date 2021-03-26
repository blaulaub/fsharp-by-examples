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

        test "verify toOrderedPresence" {
            let subSorted = Array.map List.sort
            Expect.equal ([| |]                     |> SudokuSolver.toOrderedPresence) [| |]                        "empty of zero"
            Expect.equal ([| [] |]                  |> SudokuSolver.toOrderedPresence) [| [] |]                     "empty of one"
            Expect.equal ([| [1] |]                 |> SudokuSolver.toOrderedPresence) [| [0] |]                    "one of one"
            Expect.equal ([| [1]; [2] |]            |> SudokuSolver.toOrderedPresence) [| [0]; [1] |]               "one of two each"
            Expect.equal ([| [1]; [1] |]            |> SudokuSolver.toOrderedPresence) [| [0; 1]; [] |]             "first of two twice"
            Expect.equal ([| [2]; [2] |]            |> SudokuSolver.toOrderedPresence) [| []; [0; 1] |]             "second of two twice"
            Expect.equal ([| [1;2]; [2;3]; [3;1] |] |> SudokuSolver.toOrderedPresence) [| [0; 2]; [0; 1]; [1; 2] |] "circular with two from three"
            Expect.equal ([| [1;2;3]; [2;3]; [3] |] |> SudokuSolver.toOrderedPresence) [| [0]; [0; 1]; [0; 1; 2] |] "descending with three"

            Expect.equal ([| [1]; [2]; [3]; [4]; [5]; [6] |] |> SudokuSolver.toOrderedPresence) [| [0]; [1]; [2]; [3]; [4]; [5] |] "sth a bit longer"
        }

        test "verify eliminateOption" {
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 3 3 1) [1..9] "no match removes nothing"
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 0 3 1) [2..9] "row match removes value"
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 3 0 1) [2..9] "col match removes value"
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 1 3 1) [1..9] "partial subblock match 1 removes nothing"
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 3 1 1) [1..9] "partial subblock match 2 removes nothing"
            Expect.equal (SudokuSolver.eliminateOption [1..9] 0 0 1 1 1) [2..9] "subblock match removes value"
        }

        test "verify analyse for everything possible" {
            let values =
                [|
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9];
                    [0..9]
                |]

            let rowSteps = SudokuSolver.analyse (SudokuSolver.Row { Row = 0; Values = values }) |> Seq.toList
            Expect.equal rowSteps [] "everything possible in row - no solution"

            let colSteps = SudokuSolver.analyse (SudokuSolver.Column { Col = 0; Values = values }) |> Seq.toList
            Expect.equal colSteps [] "everything possible in column - no solution"

            let blockSteps = SudokuSolver.analyse (SudokuSolver.Subblock { SupRow = 0; SupCol = 0; Values = values }) |> Seq.toList
            Expect.equal blockSteps [] "everything possible in block - no solution"
        }

        test "verify analyse for one possible" {
            let values =
                [|
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0..9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                    [0; 1; 2; 3; 4;   6; 7; 8; 9];
                |]

            let rowSteps = SudokuSolver.analyse (SudokuSolver.Row { Row = 0; Values = values }) |> Seq.toList
            Expect.equal rowSteps [SudokuSolver.ApplySingularOption { Row = 0; Col = 4; Value = 5 }] "find solution 5 in row"

            let colSteps = SudokuSolver.analyse (SudokuSolver.Column { Col = 0; Values = values }) |> Seq.toList
            Expect.equal colSteps [SudokuSolver.ApplySingularOption { Row = 4; Col = 0; Value = 5 }] "find solution 5 in columns"

            let blockSteps = SudokuSolver.analyse (SudokuSolver.Subblock { SupRow = 0; SupCol = 0; Values = values }) |> Seq.toList
            Expect.equal blockSteps [SudokuSolver.ApplySingularOption { Row = 1; Col = 1; Value = 5 }] "find solution 5 in block"
        }

        test "try solve some board" {
            let initial = easySudoku
            let final = SudokuSolver.solve initial
            SudokuSolver.toString final |> printfn "%s"
            Expect.isTrue true "dummy"
        }
    ]
