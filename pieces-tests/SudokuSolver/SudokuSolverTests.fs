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

        test "verify options" {
            let board = [| for row in 0..8 -> [| for col in 0..8 -> None |] |]
            board.[1].[2] <- Some 3

            let opts = SudokuSolver.options board

            for row in 0..8 do
            for col in 0..8 do
                match (row, col) with
                | (1, 2) ->
                    Expect.equal opts.[row].[col] [3] (sprintf "precise for row %d col %d" row col)
                | (_, _) ->
                    Expect.equal opts.[row].[col] [1..9] (sprintf "unrestricted for row %d col %d" row col)
        }

        test "find singular options" {
            let board = [| for row in 0..8 -> [| for col in 0..8 -> None |] |]
            board.[1].[2] <- Some 3

            let opts = SudokuSolver.options board

            let singularOptions = SudokuSolver.findSingularOptions opts |> Seq.toArray

            Expect.equal 1 singularOptions.Length "have only one singular option"
            Expect.equal (SudokuSolver.ApplySingularOption { Row = 1; Col = 2; Value = 3}) singularOptions.[0] "singular option matches"
        }

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

        test "verify analyse for everything possible" {
            let values =
                [|
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9];
                    [1..9]
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
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1..9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                    [1; 2; 3; 4;   6; 7; 8; 9];
                |]

            let rowSteps = SudokuSolver.analyse (SudokuSolver.Row { Row = 0; Values = values }) |> Seq.toList
            Expect.equal rowSteps [SudokuSolver.ExclussiveInRow { Numbers = [5]; RowsAndColumns = [(0, 4)] } ] "find solution 5 in row"

            let colSteps = SudokuSolver.analyse (SudokuSolver.Column { Col = 0; Values = values }) |> Seq.toList
            Expect.equal colSteps [SudokuSolver.ExclussiveInColumn { Numbers = [5]; RowsAndColumns = [(4, 0)] }] "find solution 5 in columns"

            let blockSteps = SudokuSolver.analyse (SudokuSolver.Subblock { SupRow = 0; SupCol = 0; Values = values }) |> Seq.toList
            Expect.equal blockSteps [SudokuSolver.ExclussiveInBlock { Numbers = [5]; RowsAndColumns = [(1, 1)] }] "find solution 5 in block"
        }

        test "verify applySingularOption" {

            let initialOptions () = [| for _ in 0..8 -> [| for _ in 0..8 -> [1..9] |] |]

            let singularOption : SudokuSolver.SingularOption = { Row = 0; Col = 4; Value = 5 }
            let remainingOptions = SudokuSolver.applySingularOption singularOption (initialOptions())

            for row in 0..8 do
            for col in 0..8 do
                match (row, col) with
                | (0, 4) ->
                    Expect.equal (remainingOptions.[row].[col]) [ ]                (sprintf "in row at row %d col %d" row col)
                | (0, _) ->
                    Expect.equal (remainingOptions.[row].[col]) [1;2;3;4; 6;7;8;9] (sprintf "in row at row %d col %d" row col)
                | (_, 4) ->
                    Expect.equal (remainingOptions.[row].[col]) [1;2;3;4; 6;7;8;9] (sprintf "in column at row %d col %d" row col)
                | (_, _) when (row/3=0 && col/3=1) ->
                    Expect.equal (remainingOptions.[row].[col]) [1;2;3;4; 6;7;8;9] (sprintf "in block at row %d col %d" row col)
                | _ ->
                    Expect.equal (remainingOptions.[row].[col]) [1..9]             (sprintf "elsewhere at row %d col %d" row col)
        }

        test "verify niner groups" {
            let initialOptions () = [| for _ in 0..8 -> [| for _ in 0..8 -> [1..9] |] |]
            for { Values = niner } in SudokuSolver.ninerColumns (initialOptions()) do
                Expect.equal niner [| for _ in 0..8 -> [1..9] |] "initial match"

            let singularOption : SudokuSolver.SingularOption = { Row = 0; Col = 4; Value = 5 }
            let remainingOptions = SudokuSolver.applySingularOption singularOption (initialOptions())

            let ninerRows = SudokuSolver.ninerRows remainingOptions |> Seq.toArray
            for row in 0..8 do
                let ninerRow = ninerRows.[row].Values
                Expect.equal ninerRow (
                    match row with
                    | 0 ->
                        [|
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..4]@[6..9];[]           ;[1..4]@[6..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                        |]
                    | 1 | 2 ->
                        [|
                            [1..9]       ;[1..9]       ;[1..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..9]       ;[1..9]       ;[1..9]
                        |]
                    | _ ->
                        [|
                            [1..9];[1..9]       ;[1..9]
                            [1..9];[1..4]@[6..9];[1..9]
                            [1..9];[1..9]       ;[1..9]
                        |]
                ) (sprintf "row %d after match" row)

            let ninerColumns = SudokuSolver.ninerColumns remainingOptions |> Seq.toArray
            for col in 0..8 do
                let ninerColumn = ninerColumns.[col].Values
                Expect.equal ninerColumn (
                    match col with
                    | 4 ->
                        [|
                            []           ;[1..4]@[6..9];[1..4]@[6..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                        |]
                    | 3 | 5 ->
                        [|
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..9]       ;[1..9]       ;[1..9]
                            [1..9]       ;[1..9]       ;[1..9]
                        |]
                    | _ ->
                        [|
                            [1..4]@[6..9];[1..9];[1..9]
                            [1..9]       ;[1..9];[1..9]
                            [1..9]       ;[1..9];[1..9]
                        |]
                ) (sprintf "column %d after match" col)

            let ninerSubblocks = SudokuSolver.ninerSubblocks remainingOptions |> Seq.toArray
            for block in 0..8 do
                let ninerSubblock = ninerSubblocks.[block].Values
                Expect.equal ninerSubblock (
                    match block with
                    | 1 ->
                        [|
                            [1..4]@[6..9];[]           ;[1..4]@[6..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                        |]
                    | 0 | 2 ->
                        [|
                            [1..4]@[6..9];[1..4]@[6..9];[1..4]@[6..9]
                            [1..9]       ;[1..9]       ;[1..9]
                            [1..9]       ;[1..9]       ;[1..9]
                        |]
                    | 4 | 7 ->
                        [|
                            [1..9];[1..4]@[6..9];[1..9]
                            [1..9];[1..4]@[6..9];[1..9]
                            [1..9];[1..4]@[6..9];[1..9]
                        |]
                    | _ ->
                        [|
                            [1..9];[1..9];[1..9]
                            [1..9];[1..9];[1..9]
                            [1..9];[1..9];[1..9]
                        |]
                ) (sprintf "block %d after match" block)

        }

        test "verify niner groups to ordered presence" {
            let initialOptions () = [| for _ in 0..8 -> [| for _ in 0..8 -> [1..9] |] |]
            for { Values = niner } in SudokuSolver.ninerColumns (initialOptions()) do
                Expect.equal niner [| for _ in 0..8 -> [1..9] |] "initial match"

            let singularOption : SudokuSolver.SingularOption = { Row = 0; Col = 4; Value = 5 }
            let remainingOptions = SudokuSolver.applySingularOption singularOption (initialOptions())

            let ninerRows = SudokuSolver.ninerRows remainingOptions |> Seq.toArray
            for row in 0..8 do
                let ninerRow = ninerRows.[row].Values |> SudokuSolver.toOrderedPresence
                Expect.equal ninerRow (
                    match row with
                    | 0 ->
                        [|
                            [0..3]@[5..8];[0..3]@[5..8];[0..3]@[5..8]
                            [0..3]@[5..8];[]           ;[0..3]@[5..8]
                            [0..3]@[5..8];[0..3]@[5..8];[0..3]@[5..8]
                        |]
                    | 1 | 2 ->
                        [|
                            [0..8];[0..8]       ;[0..8]
                            [0..8];[0..2]@[6..8];[0..8]
                            [0..8];[0..8]       ;[0..8]
                        |]
                    | _ ->
                        [|
                            [0..8];[0..8]       ;[0..8]
                            [0..8];[0..3]@[5..8];[0..8]
                            [0..8];[0..8]       ;[0..8]
                        |]
                ) (sprintf "row %d after match" row)

            let ninerColumns = SudokuSolver.ninerColumns remainingOptions |> Seq.toArray
            for col in 0..8 do
                let ninerColumn = ninerColumns.[col].Values |> SudokuSolver.toOrderedPresence
                Expect.equal ninerColumn (
                    match col with
                    | 4 ->
                        [|
                            [1..8];[1..8];[1..8]
                            [1..8];[]    ;[1..8]
                            [1..8];[1..8];[1..8]
                        |]
                    | 3 | 5 ->
                        [|
                            [0..8];[0..8];[0..8]
                            [0..8];[3..8];[0..8]
                            [0..8];[0..8];[0..8]
                        |]
                    | _ ->
                        [|
                            [0..8];[0..8];[0..8]
                            [0..8];[1..8];[0..8]
                            [0..8];[0..8];[0..8]
                        |]
                ) (sprintf "column %d after match" col)

            let ninerSubblocks = SudokuSolver.ninerSubblocks remainingOptions |> Seq.toArray
            for block in 0..8 do
                let ninerSubblock = ninerSubblocks.[block].Values |> SudokuSolver.toOrderedPresence
                Expect.equal ninerSubblock (
                    match block with
                    | 1 ->
                        [|
                            [0]@[2..8];[0]@[2..8];[0]@[2..8]
                            [0]@[2..8];[]        ;[0]@[2..8]
                            [0]@[2..8];[0]@[2..8];[0]@[2..8]
                        |]
                    | 0 | 2 ->
                        [|
                            [0..8];[0..8];[0..8]
                            [0..8];[3..8];[0..8]
                            [0..8];[0..8];[0..8]
                        |]
                    | 4 | 7 ->
                        [|
                            [0..8];[0..8]       ;[0..8]
                            [0..8];[0;2;3;5;6;8];[0..8]
                            [0..8];[0..8]       ;[0..8]
                        |]
                    | _ ->
                        [|
                            [0..8];[0..8];[0..8]
                            [0..8];[0..8];[0..8]
                            [0..8];[0..8];[0..8]
                        |]
                ) (sprintf "block %d after match" block)

        }

        test "try solve some board" {
            let initialState =
                SudokuSolver.initialSolutionState difficultSudoku
            do
                SudokuSolver.solveState initialState (fun state ->
                    printfn "-----------------"
                    SudokuSolver.toString state.Board |> printfn "%s"
                )
                |> ignore
            ()
        }
    ]
