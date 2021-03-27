namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A limited set of numers occupies an equally limited
/// number of fields, meaning that no other numbers can
/// be on these fields.
/// </summary>
type ExclusivePresence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}

module ExclusivePresence =

    // this code is still limited to a 9x9 (3x3) Sudoku
    let private superColumns = 3
    let private superRows = 3

    let apply (presence: ExclusivePresence) (options: Possibilities) : Possibilities =
        let total = superRows * superColumns
        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if presence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if presence.Numbers |> List.contains num then num ]
                else options.[row].[col]
            |]
        |]
