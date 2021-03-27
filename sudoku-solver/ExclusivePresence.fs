namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A limited set of numers occupies an equally limited
/// number of fields, meaning that no other numbers can
/// be on these fields.
/// </summary>
type ExclussivePresence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}
