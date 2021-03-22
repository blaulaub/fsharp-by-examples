module SudokuSolverTests

open Expecto
open SudokuSolver

let difficultBoard : KnownValues =
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

let easyBoard : KnownValues =
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

        test "try solve difficult board" {

            let initialBoard = difficultBoard

            // initially, each field in row 0..8 col 0..8 either has a precise
            // value from the initial board, or it can have any value 1..9
            let possibilities : int list array array =
                [| for row in 0..8 ->
                    [| for col in 0..8 ->
                        match initialBoard.[row].[col] with
                        | Some num -> [ num ]  // restrict to the
                        | None -> [1..9]
                    |]
                |]

            // initially, the solution is plain empty (the data from the initial
            // board is in the initial possibilities)
            let solution : int option array array =
                [| for row in 0..8 ->
                    [| for col in 0..8 ->
                        None
                    |]
                |]

            // build niner groups (rows, cols, sub-blocks) for analysis
            // note: the actual lists are returned by-reference,
            // i.e., updates are shared (visible to others)
            let ninerRowGroups =
                [|
                    for row in 0..8 -> NinerRowGroup {
                        Row = row
                        Values = [| for col in 0..8 -> possibilities.[row].[col] |]
                    }
                |]
            let ninerColGroups =
                [|
                    for col in 0..8 -> NinerColGroup {
                        Col = col
                        Values = [| for row in 0..8 -> possibilities.[row].[col] |]
                    }
                |]
            let ninerSubBlock =
                [|
                    for subRow in 0..2 do
                    for subCol in 0..2 -> NinerSubBlock {
                        SubRow = subRow
                        SubCol = subCol
                        Values = [| for row in 0..2 do for col in 0..2 -> possibilities.[row + 3*subRow].[col+3*subCol] |]
                    }
                |]

            //=============================
            // Eliminate obvious solutions
            //=============================

            // if a possibilities field contains a single value, that value must be the solution
            // (also eliminate it from all groups)
            let obviousUpdates = seq {
                for row in 0..8 do
                for col in 0..8 do
                match possibilities.[row].[col] with
                | [ num ] ->
                    yield {
                        Update = { Row = row; Col = col; Value = num }
                        Reductions = [
                            RemoveFromRow { Row = row; Value = num }
                            RemoveFromCol { Col = col; Value = num }
                            RemoveFromSubBlock { SupRow = row/3; SupCol = col/3; Value = num}
                        ]
                    }
                | _ -> ()  // do nothing
            }

            //======================
            // Analyze Niner Groups
            //======================


            let ninerGroups = seq {
                yield! ninerRowGroups
                yield! ninerColGroups
                yield! ninerSubBlock
            }

            seq {
                for ninerGroup in ninerGroups do
                    // get the values (regardless of group type) for analysis
                    let values =
                        match ninerGroup with
                        | NinerRowGroup { Values = values }
                        | NinerColGroup { Values = values }
                        | NinerSubBlock { Values = values } -> values
                    // count occurences
                    let occurences = [ for num in 1..9 -> values |> Seq.filter (fun x -> x |> List.contains num) |> Seq.length ]
                    occurences |> ignore
            }
            // for now, ignore it
            |> ignore



            Expect.isTrue true "dummy"
        }

    ]
