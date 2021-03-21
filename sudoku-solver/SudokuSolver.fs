module SudokuSolver

type Board = int option array array

let emptyBoard () : Board =
    [| for _ in 0 .. 9 -> [| for _ in 0..9 -> None |] |]

let isEmpty (board: Board) =
    board |> Seq.fold (fun state row ->
        state && row |> Seq.fold (fun state num ->
            state && match num with
                     | Some _ -> false
                     | None -> true) true) true

let getNum row col (board: Board) : int option =
    board.[row].[col]

let mergeBoards (board1: Board) (board2: Board) =
    [| for row in 0 .. 8 ->
        [| for col in 0 .. 8 ->
            match (getNum row col board1, getNum row col board2) with
            | (None, None) -> None
            | (Some num, None) -> Some num
            | (None, Some num) -> Some num
            | (Some num1, Some num2) when num1 = num2 -> Some num1
            | _ -> failwith "cannot merge boards"
        |]
    |]

let getRow row (board: Board) : int option array =
    [| for col in 0 .. 8 -> board.[row].[col] |]

let getCol col (board: Board) : int option array =
    [| for row in 0 .. 8 -> board.[row].[col] |]

let getSquare squareRow squareCol (board: Board) : int option array =
    [| for row in 0 .. 2 do for col in 0 .. 2 do yield board.[squareRow*3+row].[squareCol*3+col] |]

let getMissingForList (numbers: int option array) (expected: int list) =
    [ for num in expected do if not (numbers |> Array.contains (Some num)) then num ]

// looks vertically, horizontally and in the subsquare and returns missing possible numbers
// (this is the actual sudoku rule for insertions, but without check that the field must be empty)
let getMissingFromBoard row col (board: Board) =
    [ 1 .. 9 ]
    |> getMissingForList (getRow row board)
    |> getMissingForList (getCol col board)
    |> getMissingForList (getSquare (row/3) (col/3) board)

// looks for empty fields that have an obvious solution
// and returns only the newly found values
let obviouslyMissingOn (board: Board) : Board =
    [| for row in 0 .. 8 ->
        [| for col in 0 .. 8 ->
            match getNum row col board with
            | Some num -> None
            | None ->
                match getMissingFromBoard row col board with
                | [ num ] -> Some num
                | _ -> None
        |]
    |]

// recursive meta-solver that recursively tries to add
// missing values using some finder method
let rec trySolveWith missingFinder board =
    let missings = missingFinder board
    if (missings |> isEmpty)
    then board
    else
        let newBoard = mergeBoards board missings
        newBoard |> trySolveWith missingFinder

let trySolveWithObviouslyMissing = trySolveWith obviouslyMissingOn
