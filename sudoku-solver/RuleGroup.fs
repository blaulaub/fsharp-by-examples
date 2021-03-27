namespace Ch.PatchCode.SudokuSolver

type RuleGroup = (int*int) seq

module RuleGroup =

    let groups (): RuleGroup seq = seq {
        for row in 0..8 -> seq { for col in 0..8 -> (row, col) }
        for col in 0..8 -> seq { for row in 0..8 -> (row, col) }
        for supRow in 0..2 do
        for supCol in 0..2 -> seq {
            for subRow in 0..2 do
            for subCol in 0..2 -> (supRow*3+subRow, supCol*3+subCol)
        }
    }
