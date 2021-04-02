namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// If a numbers cannot be in fields referenced by source,
/// then they also cannot be in fields referenced by target.
/// </summary>
type CrossGroup = {
    Level: int
    Source: (int*int) seq
    Intersections: (int*int) seq seq
    Target: (int*int) seq
}
module CrossGroup =

    let private superColumnGroupsAtLevel (superRows: int) (superColumns: int) (superColumnLevel: int) = seq {

        let total = superRows * superColumns

        for supCols in MathCombinations.combinationsAtDepth superColumnLevel [0..(superColumns-1)] do
            for subRows in MathCombinations.combinationsAtDepth superColumnLevel [0..(superColumns-1)] do
            for supRow in 0..(superRows-1) do
                yield {
                    Level = superColumnLevel
                    Intersections = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (subRows |> List.contains subRow) && (supCols |> List.contains (col/superRows))
                        then yield seq {yield (supRow*superColumns + subRow, col)}
                    }
                    Source = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if (subRows |> List.contains subRow) && not (supCols |> List.contains(col/superRows))
                        then yield (supRow*superColumns + subRow, col)
                    }
                    Target = seq {
                        for subRow in 0..(superColumns-1) do
                        for col in 0..(total-1) do
                        if not (subRows |> List.contains subRow) && (supCols |> List.contains (col/superRows))
                        then yield (supRow*superColumns+subRow, col)
                    }
                }
    }

    let private superRowGroupsAtLevel (superRows: int) (superColumns: int) (superRowLevel: int) = seq {

        let total = superRows * superColumns

        for supRows in MathCombinations.combinationsAtDepth superRowLevel [0..(superRows-1)] do
            for subCols in MathCombinations.combinationsAtDepth superRowLevel [0..(superRows-1)] do
            for supCol in 0..(superColumns-1) do
                yield {
                    Level = superRowLevel
                    Intersections = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (subCols |> List.contains subCol) && (supRows |> List.contains (row/superColumns))
                        then yield seq {yield (row, supCol*superRows+subCol)}
                    }
                    Source = seq {
                        for subCol in 0..(superRows-1) do
                        for row in 0..(total-1) do
                        if (subCols |> List.contains subCol) && not (supRows |> List.contains (row/superColumns))
                        then yield (row, supCol*superRows+subCol)
                    }
                    Target = seq {
                        for row in 0..(total-1) do
                        for subCol in 0..(superRows-1) do
                        if not (subCols |> List.contains subCol) && (supRows |> List.contains (row/superColumns))
                        then yield (row, supCol*superRows+subCol)
                    }
                }
    }

    let groupsAtLevel (superRows: int) (superColumns: int) (level: int): CrossGroup seq = seq {

        if (level <= superColumns / 2)
        then yield! superColumnGroupsAtLevel (superRows: int) (superColumns: int) (level: int)

        if (level <= superRows / 2)
        then yield! superRowGroupsAtLevel (superRows: int) (superColumns: int) (level: int)
    }
