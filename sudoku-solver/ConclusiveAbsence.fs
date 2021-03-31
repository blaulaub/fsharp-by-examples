namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A list of values that are definetly not present in a list of fields.
/// </summary>
type ConclusiveAbsence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}

module ConclusiveAbsence =

    /// Returns the list of numbers present in the given fields.
    let private findPossibilities (opts: Possibilities) (fields: (int*int) seq) =
        fields
        |> Seq.map (fun (row, col) -> opts.[row].[col])
        |> Seq.concat
        |> Seq.distinct
        |> Seq.toList

    let find (group: SingularCrossGroup) (opts: Possibilities) : ConclusiveAbsence seq =
        group.Intersection
        |> findPossibilities opts
        |> Seq.filter (fun possibilities ->      group.Target |> findPossibilities opts |> List.contains possibilities)
        |> Seq.filter (fun possibilities -> not (group.Source |> findPossibilities opts |> List.contains possibilities))
        |> Seq.map (fun value -> { Numbers = [value]; RowsAndColumns = group.Target |> Seq.toList })

    let apply (superRows: int) (superColumns: int) (absence: ConclusiveAbsence) (options: Possibilities) : Possibilities =

        let total = superRows * superColumns

        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if absence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if not (absence.Numbers |> List.contains num) then num ]
                else options.[row].[col]
            |]
        |]
