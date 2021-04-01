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
        let total = opts.Length
        for row in 0..(total-1) do
        for col in 0..(total-1) do
        match opts.[row].[col] with
        | [ num ] -> yield { Row = row; Col = col; Value = num }
        | _ -> ()
    }

    let apply (superRows: int) (superColumns: int) (singularOption: SingularOption) (options: Possibilities) : Possibilities =
        let total = superRows * superColumns
        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if row = singularOption.Row && col = singularOption.Col
                then
                    []
                else
                    if row <> singularOption.Row && col <> singularOption.Col && (row/superColumns <> singularOption.Row/superColumns || col/superRows <> singularOption.Col/superRows)
                    then options.[row].[col]
                    else [ for num in options.[row].[col] do if num <> singularOption.Value then num ]
            |]
        |]
