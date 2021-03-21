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

let getMissing (numbers: int option array) =
    [ for num in 1 .. 9 do if not (numbers |> Array.contains (Some num)) then num ]
