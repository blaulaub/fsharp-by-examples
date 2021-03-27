namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A singular option is a field on the board where
/// only one value can possibly be the solution.
/// </summary>
type SingularOption = { Row: int; Col: int; Value: int }

module SingularOption =

    /// <summary>
    /// Find and return all fields that have a
    /// single possible solution.
    /// </summary>
    let find (opts: Possibilities): SingularOption seq = seq {
        for row in 0..8 do
        for col in 0..8 do
        match opts.[row].[col] with
        | [ num ] -> yield { Row = row; Col = col; Value = num }
        | _ -> ()
    }
