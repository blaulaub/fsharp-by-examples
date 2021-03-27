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
                    Expect.equal opts.[row].[col] [3] (sprintf "precise for row %d col %d" row col)
                | (_, _) ->
                    Expect.equal opts.[row].[col] [1..9] (sprintf "unrestricted for row %d col %d" row col)
        }

        test "find singular options" {
            let board = [| for row in 0..8 -> [| for col in 0..8 -> None |] |]
            board.[1].[2] <- Some 3

            let opts = Possibilities.fromBoard board

            let singularOptions = opts |> SingularOption.find |> Seq.map SolverState.ApplySingularOption |> Seq.toArray

            Expect.equal 1 singularOptions.Length "have only one singular option"
            Expect.equal (SolverState.ApplySingularOption { Row = 1; Col = 2; Value = 3}) singularOptions.[0] "singular option matches"
        }

        test "verify singularCrossGroups 1" {
            let group =
                CrossGroup.singularCrossGroups()
                |> Seq.filter (fun g -> (g.Target |> Seq.contains (1,0)) && (g.Target |> Seq.contains (2,2)))
                |> Seq.exactlyOne

            Expect.equal (group.Source |> Seq.length) 6 ""
            Expect.equal (group.Target |> Seq.length) 6 ""

            Expect.isTrue (group.Target |> Seq.contains (1,0)) ""
            Expect.isTrue (group.Target |> Seq.contains (1,1)) ""
            Expect.isTrue (group.Target |> Seq.contains (1,2)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,0)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,1)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,2)) ""

            Expect.isTrue (group.Source |> Seq.contains (0,3)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,4)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,5)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,6)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,7)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,8)) (sprintf "but contains %A" group.Source)
        }

        test "verify singularCrossGroups 2" {
            let group =
                CrossGroup.singularCrossGroups()
                |> Seq.filter (fun g -> (g.Target |> Seq.contains (0,0)) && (g.Target |> Seq.contains (2,2)))
                |> Seq.exactlyOne

            Expect.equal (group.Source |> Seq.length) 6 ""
            Expect.equal (group.Target |> Seq.length) 6 ""

            Expect.isTrue (group.Target |> Seq.contains (0,0)) ""
            Expect.isTrue (group.Target |> Seq.contains (0,1)) ""
            Expect.isTrue (group.Target |> Seq.contains (0,2)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,0)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,1)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,2)) ""

            Expect.isTrue (group.Source |> Seq.contains (1,3)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (1,4)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (1,5)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (1,6)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (1,7)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (1,8)) (sprintf "but contains %A" group.Source)
        }

        test "verify singularCrossGroups 3" {
            let group =
                CrossGroup.singularCrossGroups()
                |> Seq.filter (fun g -> (g.Target |> Seq.contains (1,3)) && (g.Target |> Seq.contains (2,5)))
                |> Seq.exactlyOne

            Expect.equal (group.Source |> Seq.length) 6 ""
            Expect.equal (group.Target |> Seq.length) 6 ""

            Expect.isTrue (group.Target |> Seq.contains (1,3)) ""
            Expect.isTrue (group.Target |> Seq.contains (1,4)) ""
            Expect.isTrue (group.Target |> Seq.contains (1,5)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,3)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,4)) ""
            Expect.isTrue (group.Target |> Seq.contains (2,5)) ""

            Expect.isTrue (group.Source |> Seq.contains (0,0)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,1)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,2)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,6)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,7)) (sprintf "but contains %A" group.Source)
            Expect.isTrue (group.Source |> Seq.contains (0,8)) (sprintf "but contains %A" group.Source)
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

        test "solve easy board" {
            let finalState = easySudoku |> SolverState.fromBoard |> SolverState.solve
            Expect.equal finalState.Board easySudokuSolution ""
        }

        test "solve difficult board" {
            let finalState = difficultSudoku |> SolverState.fromBoard |> SolverState.solve
            Expect.equal finalState.Board difficultSudokuSolution ""
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
    ]
