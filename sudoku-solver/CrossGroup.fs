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
        let superColumnCombinations = MathCombinations.combinationsAtDepth superColumnLevel [0..(superColumns-1)]

        for supCols in superColumnCombinations do
            for subRows in superColumnCombinations do
            for supRow in 0..(superRows-1) do
                yield {
                    Level = superColumnLevel
                    Intersections = seq {
                        for supCol in supCols -> seq {
                            for subCol in 0..(superRows-1) do
                            for subRow in subRows do
                            yield (supRow*superColumns+subRow, supCol*superRows+subCol)
                        }
                    }
                    Source = seq {
                        for subRow in subRows do
                        for col in 0..(total-1) do
                        if not (supCols |> List.contains(col/superRows))
                        then yield (supRow*superColumns + subRow, col)
                    }
                    Target = seq {
                        for subRow in 0..(superColumns-1) do
                        for supCol in supCols do
                        for subCol in 0..(superRows-1) do
                        if not (subRows |> List.contains subRow)
                        then yield (supRow*superColumns+subRow, supCol*superRows+subCol)
                    }
                }
    }

    let private superRowGroupsAtLevel (superRows: int) (superColumns: int) (superRowLevel: int) = seq {

        let total = superRows * superColumns
        let superRowCombinations = MathCombinations.combinationsAtDepth superRowLevel [0..(superRows-1)]

        for supRows in superRowCombinations do
            for subCols in superRowCombinations do
            for supCol in 0..(superColumns-1) do
                yield {
                    Level = superRowLevel
                    Intersections = seq {
                        for supRow in supRows -> seq {
                            for subRow in 0..(superColumns-1) do
                            for subCol in subCols do
                            yield (supRow*superColumns+subRow, supCol*superRows+subCol)
                        }
                    }
                    Source = seq {
                        for subCol in subCols do
                        for row in 0..(total-1) do
                        if not (supRows |> List.contains (row/superColumns))
                        then yield (row, supCol*superRows+subCol)
                    }
                    Target = seq {
                        for supRow in supRows do
                        for subRow in 0..(superColumns-1) do
                        for subCol in 0..(superRows-1) do
                        if not (subCols |> List.contains subCol)
                        then yield (supRow*superColumns+subRow, supCol*superRows+subCol)
                    }
                }
    }

    let groupsAtLevel (superRows: int) (superColumns: int) (level: int): CrossGroup seq = seq {

        if (level <= superColumns / 2)
        then yield! superColumnGroupsAtLevel (superRows: int) (superColumns: int) (level: int)

        if (level <= superRows / 2)
        then yield! superRowGroupsAtLevel (superRows: int) (superColumns: int) (level: int)
    }
