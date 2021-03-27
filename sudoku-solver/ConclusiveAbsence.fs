namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A list of values that are definetly not present in a list of fields.
/// </summary>
type ConclusiveAbsence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}
