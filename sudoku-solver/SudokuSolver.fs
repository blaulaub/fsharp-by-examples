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
