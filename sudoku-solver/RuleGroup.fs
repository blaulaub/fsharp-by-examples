namespace Ch.PatchCode.SudokuSolver

type RuleGroup = (int*int) seq

module RuleGroup =

    let groups (superRows: int) (superColumns: int): RuleGroup seq = seq {

        let total = superRows * superColumns

        for row in 0..8 -> seq { for col in 0..(total-1) -> (row, col) }

        for col in 0..8 -> seq { for row in 0..(total-1) -> (row, col) }

        let subRows = superColumns
        let subCols = superRows
        for supRow in 0..(superRows-1) do
        for supCol in 0..(superColumns-1) -> seq {
            for subRow in 0..(subRows-1) do
            for subCol in 0..(subCols-1) ->
                (supRow*3+subRow, supCol*3+subCol)
        }
    }
