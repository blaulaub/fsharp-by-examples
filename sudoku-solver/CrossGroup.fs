namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// If a particular number cannot be in fields referenced by source,
/// then it also cannot be in fields referenced by target.
/// </summary>
type SingularCrossGroup = {
    Source: (int*int) seq
    Target: (int*int) seq
}

module CrossGroup =

    // this code is still limited to a 9x9 (3x3) Sudoku
    let private superColumns = 3
    let private superRows = 3

    let singularCrossGroups (): SingularCrossGroup seq = seq {

        let total = superRows * superColumns

        for supCol in 0..(superColumns-1) do
        for row in 0..(total-1) do
        yield {
            Source = seq {
                for col in 0..(total-1) do
                for subRow in 0..(superColumns-1) do
                if row%superColumns = subRow && col/superRows <> supCol then yield (row, col)
            }
            Target = seq {
                for col in 0..(total-1) do
                for subRow in 0..(superColumns-1) do
                if row%superColumns <> subRow && col/superRows = supCol then yield (row, col)
            }
        }
        for supRow in 0..2 do
        for col in 0..(total-1) do
        yield {
            Source = seq {
                for row in 0..(total-1) do
                for subCol in 0..(superRows-1) do
                if col%superRows = subCol && row/superColumns <> supRow then yield (row, col)
            }
            Target = seq {
                for row in 0..(total-1) do
                for subCol in 0..(superRows-1) do
                if col%superRows <> subCol && row/superColumns = supRow then yield (row, col)
            }
        }
    }
