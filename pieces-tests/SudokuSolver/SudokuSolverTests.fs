module SudokuSolverTests

open Expecto
open SudokuSolver

/// Sample data for a difficult Sudoku
let difficultSudoku : KnownValues =
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
let easySudoku : KnownValues =
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

        test "try solve some board" {

            //==================================
            // How to create some initial state
            //==================================

            let initialState (known: KnownValues) : SolutionState =

                // initially, each field in row 0..8 col 0..8 either has a precise
                // value from the initial board, or it can have any value 1..9
                let possibilities (known: KnownValues) : Possibilities =
                    [| for row in 0..8 ->
                        [| for col in 0..8 ->
                            match known.[row].[col] with
                            | Some num -> [ num ]  // restrict to the
                            | None -> [1..9]
                        |]
                    |]

                // initially, the solution is plain empty (the data from the initial
                // board is in the initial possibilities)
                let emptySolution () : int option array array =
                    [| for row in 0..8 ->
                        [| for col in 0..8 ->
                            None
                        |]
                    |]

                { Board = emptySolution (); Possibilities = possibilities known }

            //====================================
            // How to eliminate obvious solutions
            //====================================

            // if a possibilities field contains a single value, that value must be the solution
            // (also eliminate it from all groups)
            let obviousUpdates (possibilities: Possibilities) : SolutionUpdate seq = seq {
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

            //====================================
            // How to apply an update
            //====================================

            let applySolutionUpdate (state: SolutionState) (change: SolutionUpdate) : SolutionState =

                let { Board = oldBoard; Possibilities = oldPossibilities } = state
                let { Update = update; Reductions = reductions } = change

                let newBoard =
                    let { Row = updateRow; Col = updateCol; Value = updateValue } = update
                    [| for row in 0..8 ->
                        if row <> updateRow
                        then oldBoard.[row]
                        else
                            [| for col in 0..8 ->
                                if col <> updateCol
                                then oldBoard.[row].[col]
                                else Some updateValue
                            |]
                    |]

                let applyPossibilityUpdate (oldPossibilities: Possibilities) (change: PossibilityReduction) =
                    match change with
                    | RemoveFromRow { Row = targetRow; Value = num } ->
                        [| for row in 0..8 ->
                            if row <> targetRow
                            then oldPossibilities.[row]
                            else
                                [| for col in 0..8 ->
                                    oldPossibilities.[row].[col] |> List.filter (fun x -> x <> num)
                                |]
                        |]
                    | RemoveFromCol { Col = targetCol; Value = num } ->
                        [| for row in 0..8 ->
                            [| for col in 0..8 ->
                                if col <> targetCol
                                then oldPossibilities.[row].[col]
                                else
                                    oldPossibilities.[row].[col] |> List.filter (fun x -> x <> num)
                            |]
                        |]
                    | RemoveFromSubBlock { SupRow = supRow; SupCol = supCol; Value = num } ->
                        [| for row in 0..8 ->
                            if (row < supRow*3) || (row > (supRow+1)*3-1)
                            then oldPossibilities.[row]
                            else
                                [| for col in 0..8 ->
                                    if (col < supCol*3) || (col > (supCol+1)*3-1)
                                    then oldPossibilities.[row].[col]
                                    else
                                        oldPossibilities.[row].[col] |> List.filter (fun x -> x <> num)
                                |]
                        |]

                let newPossibilities = reductions |> List.fold applyPossibilityUpdate oldPossibilities
                { Board = newBoard; Possibilities = newPossibilities }

            //====================================
            // Put this so far to the test
            //====================================

            let recoveredInitialSudoku initialSudoku =
                let state0 = initialState initialSudoku
                let { Board = _; Possibilities = possibilities0 } = state0
                let updates0 = obviousUpdates possibilities0
                let state1 = updates0 |> Seq.fold applySolutionUpdate state0
                let { Board = board1; Possibilities = _ } = state1
                board1
            Expect.equal (recoveredInitialSudoku easySudoku) easySudoku ""
            Expect.equal (recoveredInitialSudoku difficultSudoku) difficultSudoku ""


// =======================================================================================
// so far, so good..
// we have covered the case where a single-possible-value is recognized as solution
// ---
// but that's not the way
// what should be done is to eliminate possibilites
// (that comes below)
// ---
// when everything is done: cleanup and shorten the code
// ======================================================================================

            // build niner groups (rows, cols, sub-blocks) for analysis
            // note: the actual lists are returned by-reference,
            // i.e., updates are shared (visible to others)
            let ninerRowGroups (possibilities: Possibilities) =
                [|
                    for row in 0..8 -> NinerRowGroup {
                        Row = row
                        Values = [| for col in 0..8 -> possibilities.[row].[col] |]
                    }
                |]
            let ninerColGroups (possibilities: Possibilities) =
                [|
                    for col in 0..8 -> NinerColGroup {
                        Col = col
                        Values = [| for row in 0..8 -> possibilities.[row].[col] |]
                    }
                |]
            let ninerSubBlock (possibilities: Possibilities) =
                [|
                    for subRow in 0..2 do
                    for subCol in 0..2 -> NinerSubBlock {
                        SubRow = subRow
                        SubCol = subCol
                        Values = [| for row in 0..2 do for col in 0..2 -> possibilities.[row + 3*subRow].[col+3*subCol] |]
                    }
                |]


            //======================
            // Analyze Niner Groups
            //======================


            let ninerGroups (possibilities: Possibilities) = seq {
                yield! ninerRowGroups possibilities
                yield! ninerColGroups possibilities
                yield! ninerSubBlock possibilities
            }

            let somethingOf (possibilities: Possibilities) = seq {
                for ninerGroup in (ninerGroups possibilities) do
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


            Expect.isTrue true "dummy"
        }

    ]
