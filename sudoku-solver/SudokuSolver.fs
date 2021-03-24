module SudokuSolver

type Sudoku = int option array array

let print (sudoku: Sudoku) =
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

let ninerRows (opts: Options) = seq {
    for row in 0..8 -> {
        Row = row
        Values = [| for col in 0..8 -> opts.[row].[col] |]
    }
}

type NinerColumn = { Col: int; Values: int list array }

let ninerColumns (opts: Options) = seq {
    for col in 0..8 -> {
        Col = col
        Values = [| for row in 0..8 -> opts.[row].[col] |]
    }
}

type NinerSubblock = { SupRow: int; SupCol: int; Values: int list array }

let ninerSubblocks (opts: Options) = seq {
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

let ninerGroups (opts: Options) = seq {
    yield! ninerRows opts |> Seq.map Row
    yield! ninerColumns opts |> Seq.map Column
    yield! ninerSubblocks opts |> Seq.map Subblock
}

let solve sudoku : Sudoku =

    let opts = options sudoku
    let groups = ninerGroups opts

    sudoku
