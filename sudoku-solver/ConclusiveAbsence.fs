namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A list of values that are definetly not present in a list of fields.
/// </summary>
type ConclusiveAbsence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}

module ConclusiveAbsence =

    // this code is still limited to a 9x9 (3x3) Sudoku
    let private superColumns = 3
    let private superRows = 3

    let apply (absence: ConclusiveAbsence) (options: Possibilities) : Possibilities =

        let total = superRows * superColumns

        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if absence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if not (absence.Numbers |> List.contains num) then num ]
                else options.[row].[col]
            |]
        |]
