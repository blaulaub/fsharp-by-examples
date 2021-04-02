namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// If a numbers cannot be in fields referenced by source,
/// then they also cannot be in fields referenced by target.
/// </summary>
type CrossGroup = {
    Level: int
    Source: (int*int) seq
    Intersection: (int*int) seq
    Target: (int*int) seq
}
module CrossGroup =

    let private superColumnGroupsAtLevel (superRows: int) (superColumns: int) (superColumnLevel: int) = seq {

        let total = superRows * superColumns

        for row in 0..(total-1) do
            for supCol in 0..(superColumns-1) do
                yield {
                    Level = superColumnLevel
                    Intersection = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (row%superColumns = subRow) && (col/superRows = supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                    Source = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (row%superColumns = subRow) && (col/superRows <> supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                    Target = seq {
                        for col in 0..(total-1) do
                        for subRow in 0..(superColumns-1) do
                        if (row%superColumns <> subRow) && (col/superRows = supCol) then yield ((row/superColumns)*superColumns+subRow, col)
                    }
                }
    }

    let private superRowGroupsAtLevel (superRows: int) (superColumns: int) (superRowLevel: int) = seq {

        let total = superRows * superColumns
        for col in 0..(total-1) do
            for supRow in 0..(superRows-1) do
                yield {
                    Level = superRowLevel
                    Intersection = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (col%superRows = subCol) && (row/superColumns = supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                    Source = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (col%superRows = subCol) && (row/superColumns <> supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                    Target = seq {
                        for row in 0..(total-1) do
                        for subCol in 0..(superRows-1) do
                        if (col%superRows <> subCol) && (row/superColumns = supRow) then yield (row, (col/superRows)*superRows+subCol)
                    }
                }
    }

    let groupsAtLevel (superRows: int) (superColumns: int) (level: int): CrossGroup seq = seq {

        if (level <= superColumns / 2)
        then yield! superColumnGroupsAtLevel (superRows: int) (superColumns: int) (level: int)

        if (level <= superRows / 2)
        then yield! superRowGroupsAtLevel (superRows: int) (superColumns: int) (level: int)
    }
