namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// Group of fields for which the Sudoku rule (each value once) should apply.
/// </summary>
type RuleGroup = (int*int) seq

module RuleGroup =

    let private rows total = seq {
        for row in 0..(total-1) ->
            seq { for col in 0..(total-1) -> (row, col) }
    }

    let private columns total = seq {
        for col in 0..(total-1) ->
            seq { for row in 0..(total-1) -> (row, col) }
    }

    let private blocks (superRows: int) (superColumns: int) = seq {
        let subRows = superColumns
        let subCols = superRows
        for supRow in 0..(superRows-1) do
        for supCol in 0..(superColumns-1) ->
            seq {
                for subRow in 0..(subRows-1) do
                for subCol in 0..(subCols-1) ->
                    (supRow*superColumns+subRow, supCol*superRows+subCol)
            }
    }

    /// <summary>
    /// All groups where the Sudoku rule (unique value) should apply.
    /// </summary>
    /// <param name="superRows">number of sub-blocks per row</param>
    /// <param name="superColumns">number of sub-blocks per column</param>
    let groups (superRows: int) (superColumns: int): RuleGroup seq = seq {
        let total = superRows * superColumns
        yield! rows total
        yield! columns total
        yield! blocks superRows superColumns
    }
