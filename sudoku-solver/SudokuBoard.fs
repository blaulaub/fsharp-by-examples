namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// Represents the content of a Sudoku board, solved or incomplete.
/// </summary>
/// <remarks>
///  Some of the fields contain an integer value, some of them are empty.
/// </remarks>
/// <remarks>
/// Arrays and subarrays are expected to have the same size.
/// </remarks>
type SudokuBoard = int option array array

module SudokuBoard =

    let emptySudoku (superRows: int) (superColumns: int): SudokuBoard =
        let total = superRows * superColumns
        [| for _ in 0..(total-1) ->
            [| for _ in 0..(total-1) ->
                None
            |]
        |]
