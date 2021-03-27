module SolverTests

open Expecto
open Ch.PatchCode.SudokuSolver

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

            let opts = Possibilities.fromBoard board

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

            let opts = Possibilities.fromBoard board

            let singularOptions = Solver.findSingularOptions opts |> Seq.toArray

            Expect.equal 1 singularOptions.Length "have only one singular option"
            Expect.equal (Solver.ApplySingularOption { Row = 1; Col = 2; Value = 3}) singularOptions.[0] "singular option matches"
        }

        test "verify toOrderedPresence" {
            let subSorted = Array.map List.sort
            Expect.equal ([| |]                     |> Solver.toOrderedPresence) [| |]                        "empty of zero"
            Expect.equal ([| [] |]                  |> Solver.toOrderedPresence) [| [] |]                     "empty of one"
            Expect.equal ([| [1] |]                 |> Solver.toOrderedPresence) [| [0] |]                    "one of one"
            Expect.equal ([| [1]; [2] |]            |> Solver.toOrderedPresence) [| [0]; [1] |]               "one of two each"
            Expect.equal ([| [1]; [1] |]            |> Solver.toOrderedPresence) [| [0; 1]; [] |]             "first of two twice"
            Expect.equal ([| [2]; [2] |]            |> Solver.toOrderedPresence) [| []; [0; 1] |]             "second of two twice"
            Expect.equal ([| [1;2]; [2;3]; [3;1] |] |> Solver.toOrderedPresence) [| [0; 2]; [0; 1]; [1; 2] |] "circular with two from three"
            Expect.equal ([| [1;2;3]; [2;3]; [3] |] |> Solver.toOrderedPresence) [| [0]; [0; 1]; [0; 1; 2] |] "descending with three"

            Expect.equal ([| [1]; [2]; [3]; [4]; [5]; [6] |] |> Solver.toOrderedPresence) [| [0]; [1]; [2]; [3]; [4]; [5] |] "sth a bit longer"
        }

        test "verify applySingularOption" {

            let initialOptions () = [| for _ in 0..8 -> [| for _ in 0..8 -> [1..9] |] |]

            let singularOption : SingularOption = { Row = 0; Col = 4; Value = 5 }
            let remainingOptions = SingularOption.apply singularOption (initialOptions())

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

        test "try solve some board" {
            let initialState =
                Solver.initialSolutionState difficultSudoku
            do
                Solver.solveState initialState (fun state ->
                    printfn "-----------------"
                    Utilities.toString state.Board |> printfn "%s"
                )
                |> ignore
            ()
        }
    ]
