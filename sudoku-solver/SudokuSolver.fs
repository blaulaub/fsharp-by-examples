module SudokuSolver

type RowAndColumnBoard<'T> = 'T array array

let indices (data: 'T array) : int seq = seq { 0..(data.Length-1) }

let mapForRowAndColBy<'In, 'Out> action (source: 'In RowAndColumnBoard): 'Out RowAndColumnBoard =
    [| for row in indices source ->
        [| for col in indices source.[row] ->
            action source row col
        |]
    |]

let mapEachFieldBy<'In, 'Out> action (source: 'In RowAndColumnBoard): 'Out RowAndColumnBoard =
    mapForRowAndColBy (fun source row col -> action source.[row].[col]) source

let boardToString<'T> (fieldToString: 'T -> string) (board: 'T RowAndColumnBoard) =
    board
    |> Seq.map (fun line ->
        line
        |> Seq.map fieldToString
        |> String.concat ""
        )
    |> String.concat "\n"

/// <summary>
/// A Sudoku board. Some of the fields contain an integer value,
/// some of them are empty.
/// </summary>
type Sudoku = int option RowAndColumnBoard

/// <summary>
/// Dump the Sudoku into a nine-line-nine-column string.
/// </summary>
let toString = boardToString (fun x ->
    match x with
    | Some value -> sprintf " %d " value
    | None -> "   "
    )

/// <summary>
/// A Sudoku board not with the solution, but with the possible
/// numbers.
/// </summary>
/// <remarks>
/// Instead of an algorithm trying to make <see cref="Sudoku"/> complete,
/// we will look for an algorithm trying to make <see cref="Options"/> empty.
/// </remarks>
type Options = int list RowAndColumnBoard

/// <summary>
/// Derives initial options from a given <see cref="Sudoku"/> setup.
/// </summary>
let options (sudoku: Sudoku): Options =
    sudoku
    |> mapEachFieldBy (fun field ->
            match field with
            | Some num -> [ num ]
            | None -> [1..9]
        )

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

type NinerGroup =
| Row of NinerRow
| Column of NinerColumn
| Subblock of NinerSubblock

let ninerGroups (opts: Options): NinerGroup seq = seq {
    yield! ninerRows opts |> Seq.map Row
    yield! ninerColumns opts |> Seq.map Column
    yield! ninerSubblocks opts |> Seq.map Subblock
}

/// <summary>
/// A singular option is a field on the board where
/// only one value can possibly be the solution.
/// </summary>
type SingularOption = { Row: int; Col: int; Value: int }

/// <summary>
/// A limited number of values occupies a limited number
/// of fields, meaning that no other numbers can be on
/// these fields.
/// </summary>
type ExclussivePresence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
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

/// <summary>
/// Union of all considered kinds of solution steps.
/// </summary>
type SolutionStep =
| ApplySingularOption of SingularOption
| SingleInRow of ExclussivePresence
| SingleInColumn of ExclussivePresence
| SingleInBlock of ExclussivePresence

/// <summary>
/// Find and return all fields that have a
/// single possible solution.
/// </summary>
let findSingularOptions (opts: Options): SolutionStep seq = seq {
    for row in 0..8 do
    for col in 0..8 do
    match opts.[row].[col] with
    | [ num ] -> yield ApplySingularOption { Row = row; Col = col; Value = num }
    | _ -> ()
}

let applySingularOption { Row = targetRow; Col = targetCol; Value = targetNum } (options: Options) : Options =
    [| for row in 0..8 ->
        [| for col in 0..8 ->
            if row = targetRow && col = targetCol
            then
                []
            else
                if row <> targetRow && col <> targetCol && (row/3 <> targetRow/3 || col/3 <> targetCol/3)
                then options.[row].[col]
                else [ for num in options.[row].[col] do if num <> targetNum then num ]
        |]
    |]

let applySingularOptionToState { Row = targetRow; Col = targetCol; Value = num } oldState =
        printfn "field row %d col %d has single option %d" targetRow targetCol num
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
            oldOptions
            |> applySingularOption { Row = targetRow; Col = targetCol; Value = num }
        { Board = newBoard; Options = newOptions }

let applyStep (oldState: SolutionState) (step: SolutionStep): SolutionState =
    match step with
    | ApplySingularOption option ->
        printfn "(singular option)"
        applySingularOptionToState option oldState
    | SingleInRow { Numbers = [value]; RowsAndColumns = [(row, col)] } ->
        printfn "(singular in row)"
        applySingularOptionToState {Row = row; Col = col; Value = value} oldState
    | SingleInColumn { Numbers = [value]; RowsAndColumns = [(row, col)] } ->
        printfn "(singular in column)"
        applySingularOptionToState {Row = row; Col = col; Value = value} oldState
    | SingleInBlock { Numbers = [value]; RowsAndColumns = [(row, col)] } ->
        printfn "(singular in block)"
        applySingularOptionToState {Row = row; Col = col; Value = value} oldState
    | _ -> failwith (sprintf "unsupported %A" step)

let toOrderedPresence (values: int list array) : int list array =
    let up = values.Length - 1
    values
    |> Array.rev
    |> Array.fold (fun (i, state) l ->
        (
            i-1,
            l |> List.fold (fun state v ->
                // note: we may create a lots of arrays here, maybe that is not cheap...
                [|
                    for idx in 0..up ->
                        if idx = (v-1)
                        then i::state.[idx]
                        else state.[idx]
                |]
            ) state
        )
    ) (up, [| for _ in 0..up -> [] |])
    |> snd

let matchSinglePresence (mapper: int -> int -> SolutionStep) (presence: int list array) = seq {
    let up = presence.Length - 1
    for value in 0..up do
        match presence.[value] with
        | [ position ] -> yield mapper position (value+1)
        | _ -> ()
    }

let analyse (group : NinerGroup) : SolutionStep seq =
    match group with
        | Row { Row = targetRow; Values = values } ->
            values
            |> toOrderedPresence
            |> matchSinglePresence (fun pos value -> SingleInRow { Numbers = [value]; RowsAndColumns = [(targetRow, pos)] })
        | Column { Col = targetCol; Values = values } ->
            values
            |> toOrderedPresence
            |> matchSinglePresence (fun pos value -> SingleInColumn { Numbers = [value]; RowsAndColumns = [(pos, targetCol)] })
        | Subblock { SupRow = supRow; SupCol = supCol; Values = values } ->
            values
            |> toOrderedPresence
            |> matchSinglePresence (fun pos value -> SingleInBlock { Numbers = [value]; RowsAndColumns = [(supRow*3+pos/3, supCol*3+pos%3)] })

let steps options = seq {
    // first try eliminating singular options
    yield! findSingularOptions options
    // next try checking groups
    for group in ninerGroups options do
        yield! analyse group
}

let rec solveState state preAction =
    preAction state
    let next =
        steps state.Options
        |> Seq.tryHead
        |> Option.map (applyStep state)
    match next with
    | Some newState -> solveState newState preAction
    | None -> state
