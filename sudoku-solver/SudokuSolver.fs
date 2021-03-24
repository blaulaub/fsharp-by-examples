module SudokuSolver

type Sudoku = int option array array

let print (sudoku: Sudoku): unit =
    let multilineOutput =
        sudoku
        |> Seq.map (fun line ->
            line
            |> Seq.map (fun field ->
                match field with
                | Some digit -> sprintf "%d" digit
                | None -> " ")
            |> String.concat " "
            )
        |> String.concat "\n"
    printfn "%s" multilineOutput

type Options = int list array array

let options (sudoku: Sudoku): Options =
    [| for row in 0..8 ->
        [| for col in 0..8 ->
            match sudoku.[row].[col] with
            | Some num -> [ num ]
            | None -> [ 1..9]
        |]
    |]

type NinerRow = { Row: int; Values: int list array }

let ninerRows (opts: Options): NinerRow seq = seq {
    for row in 0..8 -> {
        Row = row
        Values = [| for col in 0..8 -> opts.[row].[col] |]
    }
}

type NinerColumn = { Col: int; Values: int list array }

let ninerColumns (opts: Options): NinerColumn seq = seq {
    for col in 0..8 -> {
        Col = col
        Values = [| for row in 0..8 -> opts.[row].[col] |]
    }
}

type NinerSubblock = { SupRow: int; SupCol: int; Values: int list array }

let ninerSubblocks (opts: Options): NinerSubblock seq = seq {
    for supRow in 0..2 do
    for supCol in 0..2 -> {
        SupRow = supRow
        SupCol = supCol
        Values = [| for row in 0..2 do for col in 0..2 -> opts.[3*supRow + row].[3*supCol + col] |]
    }
}

type NinerGroups =
| Row of NinerRow
| Column of NinerColumn
| Subblock of NinerSubblock

let ninerGroups (opts: Options): NinerGroups seq = seq {
    yield! ninerRows opts |> Seq.map Row
    yield! ninerColumns opts |> Seq.map Column
    yield! ninerSubblocks opts |> Seq.map Subblock
}

type SingularOption = { Row: int; Col: int; Value: int }

let singularOptions (opts: Options): SingularOption seq = seq {
    for row in 0..8 do
    for col in 0..8 do
    match opts.[row].[col] with
    | [ num ] -> yield { Row = row; Col = col; Value = num }
    | _ -> ()
}

type SolutionState = { Board: Sudoku; Options: Options }

let emptySudoku (): Sudoku =
    [| for row in 0..8 ->
        [| for col in 0..8 ->
            None
        |]
    |]

let initialSolutionState (sudoku: Sudoku): SolutionState =
    {
        Board = emptySudoku ()
        Options = options sudoku
    }

type SolutionStep =
| ApplySingularOption of SingularOption

let applyStep (step: SolutionStep) (oldState: SolutionState): SolutionState =
    match step with
    | ApplySingularOption { Row = targetRow; Col = targetCol; Value = num } ->
        let oldBoard = oldState.Board
        let newBoard =
            [| for row in 0..8 ->
                if row <> targetRow
                then oldBoard.[row]
                else
                    [| for col in 0..8 ->
                        if col <> targetCol
                        then oldBoard.[row].[col]
                        else Some num
                    |]
            |]
        let oldOptions = oldState.Options
        let newOptions =
            oldOptions // not correct; remove from row, col, subgroup

        { Board = newBoard; Options = newOptions }

let solve sudoku : Sudoku =

    let initialState = initialSolutionState sudoku

    let singularOptions = singularOptions initialState.Options

    let groups = ninerGroups initialState.Options

    sudoku
