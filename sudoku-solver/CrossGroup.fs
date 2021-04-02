namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// If a particular number cannot be in fields referenced by source,
/// then it also cannot be in fields referenced by target.
/// </summary>
type SingularCrossGroup = {
    Source: (int*int) seq
    Intersection: (int*int) seq
    Target: (int*int) seq
}

module CrossGroup =

    let groups (superRows: int) (superColumns: int): SingularCrossGroup seq = seq {

        let total = superRows * superColumns

        for row in 0..(total-1) do
            for supCol in 0..(superColumns-1) do
                yield {
                    Source = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (row%superColumns = subRow) && (col/superRows <> supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                    Intersection = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (row%superColumns = subRow) && (col/superRows = supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                    Target = seq {
                        for col in 0..(total-1) do
                        for subRow in 0..(superColumns-1) do
                        if (row%superColumns <> subRow) && (col/superRows = supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                }

        for col in 0..(total-1) do
            for supRow in 0..(superRows-1) do
                yield {
                    Source = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (col%superRows = subCol) && (row/superColumns <> supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                    Intersection = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (col%superRows = subCol) && (row/superColumns = supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                    Target = seq {
                        for row in 0..(total-1) do
                        for subCol in 0..(superRows-1) do
                        if (col%superRows <> subCol) && (row/superColumns = supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                }
    }
