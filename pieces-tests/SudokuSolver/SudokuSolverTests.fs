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

let difficultSudoku3 =
    [|
        [| Some 4; None  ; None  ; Some 3; Some 1; None  ; None  ; None  ; None   |];
        [| Some 3; None  ; None  ; None  ; None  ; None  ; Some 8; None  ; Some 4 |];
        [| Some 5; Some 9; None  ; None  ; None  ; Some 6; Some 3; None  ; Some 1 |];
        [| None  ; None  ; Some 4; None  ; Some 6; Some 5; None  ; Some 3; None   |];
        [| Some 9; None  ; None  ; None  ; None  ; Some 3; None  ; None  ; None   |];
        [| None  ; None  ; Some 5; None  ; Some 7; None  ; Some 1; None  ; None   |];
        [| Some 2; Some 8; None  ; None  ; Some 4; None  ; None  ; None  ; Some 3 |];
        [| Some 6; None  ; None  ; None  ; None  ; Some 1; Some 4; None  ; Some 9 |];
        [| None  ; None  ; None  ; None  ; Some 9; None  ; Some 5; Some 8; None   |]
    |]

let difficultSudoku3Solution =
    [|
        [| Some 4; Some 7; Some 6; Some 3; Some 1; Some 8; Some 9; Some 5; Some 2 |];
        [| Some 3; Some 2; Some 1; Some 7; Some 5; Some 9; Some 8; Some 6; Some 4 |];
        [| Some 5; Some 9; Some 8; Some 4; Some 2; Some 6; Some 3; Some 7; Some 1 |];
        [| Some 7; Some 1; Some 4; Some 9; Some 6; Some 5; Some 2; Some 3; Some 8 |];
        [| Some 9; Some 6; Some 2; Some 1; Some 8; Some 3; Some 7; Some 4; Some 5 |];
        [| Some 8; Some 3; Some 5; Some 2; Some 7; Some 4; Some 1; Some 9; Some 6 |];
        [| Some 2; Some 8; Some 9; Some 5; Some 4; Some 7; Some 6; Some 1; Some 3 |];
        [| Some 6; Some 5; Some 7; Some 8; Some 3; Some 1; Some 4; Some 2; Some 9 |];
        [| Some 1; Some 4; Some 3; Some 6; Some 9; Some 2; Some 5; Some 8; Some 7 |]
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

            let opts = Possibilities.fromBoard board

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

            let opts = Possibilities.fromBoard board

            let singularOptions = opts |> SingularOption.find |> Seq.map SolverState.ApplySingularOption |> Seq.toArray

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
            let finalState = easySudoku |> SolverState.fromBoard |> SolverState.solve 3 3
            Expect.equal finalState.Board easySudokuSolution ""
        }

        test "solve difficult board" {
            let finalState = difficultSudoku |> SolverState.fromBoard |> SolverState.solve 3 3
            Expect.equal finalState.Board difficultSudokuSolution ""
        }

        test "solve difficult board 2" {
            let finalState = difficultSudoku2 |> SolverState.fromBoard |> SolverState.solve 3 3
            Expect.equal finalState.Board difficultSudoku2Solution ""
        }

        test "solve difficult board 3" {
            let finalState = difficultSudoku3 |> SolverState.fromBoard |> SolverState.solve 3 3
            Expect.equal finalState.Board difficultSudoku3Solution ""
        }

        test "try invent some board" {

            let superRows = 4
            let superCols = 3
            let total = superRows * superCols

            let rnd = System.Random()
            let nextRandom upperEx = rnd.Next(upperEx)

            let board = Inventor.invent superRows superCols nextRandom

            let hintCount (board: Board) =
                board
                |> Array.concat
                |> Array.fold (fun sum field -> if field.IsSome then sum+1 else sum) 0

            printfn "Hint Count: %d" (hintCount board)
            printfn "---"
            Utilities.toString board |> printfn "%s"
            printfn "---"
            SolverState.fromBoard board
            |> SolverState.solve superRows superCols
            |> (fun state -> state.Board)
            |> Utilities.toString
            |> printfn "%s"

        }
    ]
