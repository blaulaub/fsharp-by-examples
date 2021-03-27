namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// Board of fields that make up a Sudoku board.
/// </summary>
type Board = int option array array

module Board =

    /// <summary>
    /// An empty board.
    /// </summary>
    /// <param name="superRows">number of sub-blocks per row</param>
    /// <param name="superColumns">number of sub-blocks per column</param>
    let empty (superRows: int) (superColumns: int): Board =

        let total = superRows * superColumns

        [| for _ in 0..(total-1) ->
            [| for _ in 0..(total-1) ->
                None
            |]
        |]
