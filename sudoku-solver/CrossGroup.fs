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

    let singularCrossGroups (): SingularCrossGroup seq = seq {
        for supCol in 0..2 do
        for row in 0..8 do
        yield {
            Source = seq {
                for col in 0..8 do
                for subRow in 0..2 do
                if row%3 = subRow && col/3 <> supCol then yield (row, col)
            }
            Target = seq {
                for col in 0..8 do
                for subRow in 0..2 do
                if row%3 <> subRow && col/3 = supCol then yield (row, col)
            }
        }
        for supRow in 0..2 do
        for col in 0..8 do
        yield {
            Source = seq {
                for row in 0..8 do
                for subCol in 0..2 do
                if col%3 = subCol && row/3 <> supRow then yield (row, col)
            }
            Target = seq {
                for row in 0..8 do
                for subCol in 0..2 do
                if col%3 <> subCol && row/3 = supRow then yield (row, col)
            }
        }
    }
