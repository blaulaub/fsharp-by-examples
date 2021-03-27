namespace Ch.PatchCode.SudokuSolver

type Board = int option array array

module Board =

    let empty (superRows: int) (superColumns: int): Board =

        let total = superRows * superColumns

        [| for _ in 0..(total-1) ->
            [| for _ in 0..(total-1) ->
                None
            |]
        |]
