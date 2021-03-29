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

let difficultSudokuSolution =
    [|
        [| Some 2; Some 8; Some 9; Some 4; Some 3; Some 6; Some 1; Some 5; Some 7 |];
        [| Some 6; Some 1; Some 3; Some 5; Some 9; Some 7; Some 8; Some 4; Some 2 |];
        [| Some 5; Some 7; Some 4; Some 1; Some 8; Some 2; Some 9; Some 6; Some 3 |];
        [| Some 4; Some 5; Some 7; Some 3; Some 1; Some 8; Some 6; Some 2; Some 9 |];
        [| Some 3; Some 9; Some 2; Some 6; Some 4; Some 5; Some 7; Some 8; Some 1 |];
        [| Some 1; Some 6; Some 8; Some 2; Some 7; Some 9; Some 5; Some 3; Some 4 |];
        [| Some 9; Some 4; Some 6; Some 7; Some 5; Some 3; Some 2; Some 1; Some 8 |];
        [| Some 8; Some 3; Some 5; Some 9; Some 2; Some 1; Some 4; Some 7; Some 6 |];
        [| Some 7; Some 2; Some 1; Some 8; Some 6; Some 4; Some 3; Some 9; Some 5 |]
    |]

/// Sample data for another difficult Sudoku
let difficultSudoku2 =
    [|
        [| None  ; Some 4; None  ; None  ; Some 1; None  ; None  ; Some 9; None   |];
        [| Some 2; None  ; None  ; Some 7; None  ; Some 8; None  ; None  ; Some 1 |];
        [| None  ; None  ; None  ; Some 3; None  ; Some 4; None  ; None  ; None   |];
        [| None  ; Some 6; Some 5; None  ; None  ; None  ; Some 7; Some 4; None   |];
        [| Some 4; None  ; None  ; None  ; None  ; None  ; None  ; None  ; Some 3 |];
        [| None  ; Some 8; Some 3; None  ; None  ; None  ; Some 9; Some 5; None   |];
        [| None  ; None  ; None  ; Some 8; None  ; Some 3; None  ; None  ; None   |];
        [| Some 8; None  ; None  ; Some 5; None  ; Some 9; None  ; None  ; Some 4 |];
        [| None  ; Some 3; None  ; None  ; Some 4; None  ; None  ; Some 6; None   |]
    |]

let difficultSudoku2Solution =
    [|
        [| Some 3; Some 4; Some 8; Some 6; Some 1; Some 2; Some 5; Some 9; Some 7 |];
        [| Some 2; Some 5; Some 6; Some 7; Some 9; Some 8; Some 4; Some 3; Some 1 |];
        [| Some 7; Some 1; Some 9; Some 3; Some 5; Some 4; Some 2; Some 8; Some 6 |];
        [| Some 9; Some 6; Some 5; Some 2; Some 3; Some 1; Some 7; Some 4; Some 8 |];
        [| Some 4; Some 2; Some 7; Some 9; Some 8; Some 5; Some 6; Some 1; Some 3 |];
        [| Some 1; Some 8; Some 3; Some 4; Some 7; Some 6; Some 9; Some 5; Some 2 |];
        [| Some 6; Some 9; Some 4; Some 8; Some 2; Some 3; Some 1; Some 7; Some 5 |];
        [| Some 8; Some 7; Some 1; Some 5; Some 6; Some 9; Some 3; Some 2; Some 4 |];
        [| Some 5; Some 3; Some 2; Some 1; Some 4; Some 7; Some 8; Some 6; Some 9 |]
    |]

// Sample data for an easy Sudoku
let easySudoku =
    [|
        [| Some 5; Some 4; None  ; None  ; Some 3; None  ; None  ; Some 9; Some 2 |];
        [| Some 2; None  ; Some 3; Some 7; None  ; Some 5; Some 1; None  ; Some 6 |];
        [| None  ; Some 9; None  ; Some 4; None  ; Some 6; None  ; Some 5; None   |];
        [| None  ; Some 2; Some 9; None  ; None  ; None  ; Some 5; Some 1; None   |];
        [| Some 1; None  ; None  ; None  ; None  ; None  ; None  ; None  ; Some 9 |];
        [| None  ; Some 7; Some 5; None  ; None  ; None  ; Some 3; Some 6; None   |];
        [| None  ; Some 1; None  ; Some 2; None  ; Some 9; None  ; Some 3; None   |];
        [| Some 9; None  ; Some 7; Some 8; None  ; Some 3; Some 4; None  ; Some 1 |];
        [| Some 3; Some 5; None  ; None  ; Some 4; None  ; None  ; Some 7; Some 8 |]
    |]

let easySudokuSolution =
    [|
        [| Some 5; Some 4; Some 6; Some 1; Some 3; Some 8; Some 7; Some 9; Some 2 |];
        [| Some 2; Some 8; Some 3; Some 7; Some 9; Some 5; Some 1; Some 4; Some 6 |];
        [| Some 7; Some 9; Some 1; Some 4; Some 2; Some 6; Some 8; Some 5; Some 3 |];
        [| Some 6; Some 2; Some 9; Some 3; Some 8; Some 4; Some 5; Some 1; Some 7 |];
        [| Some 1; Some 3; Some 4; Some 5; Some 6; Some 7; Some 2; Some 8; Some 9 |];
        [| Some 8; Some 7; Some 5; Some 9; Some 1; Some 2; Some 3; Some 6; Some 4 |];
        [| Some 4; Some 1; Some 8; Some 2; Some 7; Some 9; Some 6; Some 3; Some 5 |];
        [| Some 9; Some 6; Some 7; Some 8; Some 5; Some 3; Some 4; Some 2; Some 1 |];
        [| Some 3; Some 5; Some 2; Some 6; Some 4; Some 1; Some 9; Some 7; Some 8 |]
    |]

[<Tests>]
let tests =
    testList "Sudoku solver tests" [

        test "verify options" {
            let board = [| for row in 0..8 -> [| for col in 0..8 -> None |] |]
            board.[1].[2] <- Some 3

            let opts = Possibilities.fromBoard 3 3 board

            for row in 0..8 do
            for col in 0..8 do
                match (row, col) with
                | (1, 2) ->
                    Expect.equal opts.[row].[col] [2] (sprintf "precise for row %d col %d" row col)
                | (_, _) ->
                    Expect.equal opts.[row].[col] [0..8] (sprintf "unrestricted for row %d col %d" row col)
        }

        test "find singular options" {
            let board = [| for row in 0..8 -> [| for col in 0..8 -> None |] |]
            board.[1].[2] <- Some 3

            let opts = Possibilities.fromBoard 3 3 board

            let singularOptions = opts |> SingularOption.find 3 3 |> Seq.map SolverState.ApplySingularOption |> Seq.toArray

            Expect.equal 1 singularOptions.Length "have only one singular option"
            Expect.equal (SolverState.ApplySingularOption { Row = 1; Col = 2; Value = 2}) singularOptions.[0] "singular option matches"
        }

        test "verify applySingularOption" {

            let initialOptions () = [| for _ in 0..8 -> [| for _ in 0..8 -> [0..8] |] |]

            let singularOption : SingularOption = { Row = 0; Col = 4; Value = 4 }
            let remainingOptions = SingularOption.apply 3 3 singularOption (initialOptions())

            for row in 0..8 do
            for col in 0..8 do
                match (row, col) with
                | (0, 4) ->
                    Expect.equal (remainingOptions.[row].[col]) [ ]                (sprintf "in row at row %d col %d" row col)
                | (0, _) ->
                    Expect.equal (remainingOptions.[row].[col]) [0;1;2;3; 5;6;7;8] (sprintf "in row at row %d col %d" row col)
                | (_, 4) ->
                    Expect.equal (remainingOptions.[row].[col]) [0;1;2;3; 5;6;7;8] (sprintf "in column at row %d col %d" row col)
                | (_, _) when (row/3=0 && col/3=1) ->
                    Expect.equal (remainingOptions.[row].[col]) [0;1;2;3; 5;6;7;8] (sprintf "in block at row %d col %d" row col)
                | _ ->
                    Expect.equal (remainingOptions.[row].[col]) [0..8]             (sprintf "elsewhere at row %d col %d" row col)
        }

        test "solve easy board" {
            let finalState = easySudoku |> SolverState.fromBoard |> SolverState.solve
            Expect.equal finalState.Board easySudokuSolution ""
        }

        test "solve difficult board" {
            let finalState = difficultSudoku |> SolverState.fromBoard |> SolverState.solve
            Expect.equal finalState.Board difficultSudokuSolution ""
        }

        test "solve difficult board 2" {
            let finalState = difficultSudoku2 |> SolverState.fromBoard |> SolverState.solve
            Expect.equal finalState.Board difficultSudoku2Solution ""
        }

        test "try solve some board" {
            let initialState =
                SolverState.fromBoard difficultSudoku2

            initialState
            |> SolverState.solveWithPreAction (fun state ->
                printfn "---------------------------"
                Utilities.toString state.Board |> printfn "%s"
                )
            |> ignore
        }

        test "try invent some board" {

            let apply (row, col, n) next0 =
                next0
                |> SolverState.applySingularOptionToState { Row = row; Col = col; Value = n}

            let next (rnd: int -> int) state =

                let next0 =
                    state
                    |> SolverState.solve

                let undet = [|
                    for row in 0..8 do
                    for col in 0..8 do
                    if next0.Options.[row].[col].Length > 0
                    then yield (row, col)
                |]

                let pick =
                    if undet.Length > 0
                    then
                        let (row, col) = undet.[rnd undet.Length]
                        let l = rnd next0.Options.[row].[col].Length
                        let n = next0.Options.[row].[col].[l]
                        Some (row, col, n)
                    else None

                match pick with
                | Some pick -> apply pick next0
                | None -> next0

            let initialState = Board.empty 3 3 |> SolverState.fromBoard
            let rnd = System.Random()
            let nextRandom upperEx = rnd.Next(upperEx)

            let next1 =
                initialState
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom
                |> next nextRandom

            Utilities.toString next1.Board |> printfn "%s"
        }
    ]
