namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// Board of fields that make up a Sudoku board.
/// </summary>
type Board = int option array array

module Board =

    /// <summary>
    /// An empty board.
    /// </summary>
    let empty (total: int): Board =
        [| for _ in 0..(total-1) ->
            [| for _ in 0..(total-1) ->
                None
            |]
        |]
