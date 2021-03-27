namespace Ch.PatchCode.SudokuSolver

type SudokuBoard = int option array array

module SudokuBoard =

    let emptySudoku (superRows: int) (superColumns: int): SudokuBoard =
        let total = superRows * superColumns
        [| for _ in 0..(total-1) ->
            [| for _ in 0..(total-1) ->
                None
            |]
        |]
