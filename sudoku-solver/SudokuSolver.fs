module SudokuSolver

type Board = int option array array

let emptyBoard () : Board =
    [| for _ in 0 .. 9 -> [| for _ in 0..9 -> None |] |]

let setNum row col num (board: Board) : Board =
    let copy = Array.copy board
    copy.[row] <- Array.copy copy.[row]
    copy.[row].[col] <- num
    copy

let getNum row col (board: Board) : int option =
    board.[row].[col]

let getRow row (board: Board) : int option array =
    [| for col in 0 .. 8 -> board.[row].[col] |]

let getCol col (board: Board) : int option array =
    [| for row in 0 .. 8 -> board.[row].[col] |]

let getSquare squareRow squareCol (board: Board) : int option array =
    [| for row in 0 .. 2 do for col in 0 .. 2 do yield board.[squareRow*3+row].[squareCol*3+col] |]

let getMissingForList (numbers: int option array) (expected: int list) =
    [ for num in expected do if not (numbers |> Array.contains (Some num)) then num ]

// looks vertically, horizontally and in the subsquare and returns missing possible numbers
let getMissingFromBoard row col (board: Board) =
    [ 1 .. 9 ]
    |> getMissingForList (getRow row board)
    |> getMissingForList (getCol col board)
    |> getMissingForList (getSquare (row/3) (col/3) board)

// looks for empty fields that have an obvious solution
// and returns only the newly found values
let obviouslyMissingOn (board: Board) =
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
