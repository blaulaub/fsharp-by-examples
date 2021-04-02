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

    let find (group: CrossGroup) (opts: Possibilities) : ConclusiveAbsence seq =
        group.Intersections
        |> Seq.concat   // TODO: each intersection must contain the number searched for
        |> findPossibilities opts
        |> MathCombinations.combinationsAtDepth 1
        |> Seq.filter (fun values ->      group.Target |> findPossibilities opts |> (fun present -> values |> Seq.forall (fun value -> present |> List.contains value)))
        |> Seq.filter (fun values -> not (group.Source |> findPossibilities opts |> (fun present -> values |> Seq.forall (fun value -> present |> List.contains value))))
        |> Seq.map (fun values -> { Numbers = values; RowsAndColumns = group.Target |> Seq.toList })

    let apply (absence: ConclusiveAbsence) (options: Possibilities) : Possibilities =
        let total = options.Length
        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if absence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if not (absence.Numbers |> List.contains num) then num ]
                else options.[row].[col]
            |]
        |]
