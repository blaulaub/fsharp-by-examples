namespace Ch.PatchCode.SudokuSolver

type Possibilities = int list array array

module Possibilities =

    /// <summary>
    /// Derives initial options from a given <see cref="Board"/> setup.
    /// </summary>
    let fromBoard (board: Board): Possibilities =

        let total = board.Length

        [| for row in board ->
            [| for field in row ->
                match field with
                | Some num -> [ num ]
                | None -> [1..total]
             |]
        |]
