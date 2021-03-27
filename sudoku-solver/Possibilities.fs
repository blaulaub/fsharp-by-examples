namespace Ch.PatchCode.SudokuSolver

type Possibilities = int list array array

module Possibilities =

    /// <summary>
    /// Derives initial options from a given <see cref="Sudoku"/> setup.
    /// </summary>
    let fromBoard (sudoku: Board): Possibilities =
        [| for row in sudoku ->
            [| for field in row ->
                match field with
                | Some num -> [ num ]
                | None -> [1..9]
             |]
        |]
