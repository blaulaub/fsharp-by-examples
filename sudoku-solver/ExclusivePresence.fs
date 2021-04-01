namespace Ch.PatchCode.SudokuSolver

/// <summary>
/// A limited set of numers occupies an equally limited
/// number of fields, meaning that no other numbers can
/// be on these fields.
/// </summary>
type ExclusivePresence = {
    Numbers: int list
    RowsAndColumns: (int * int) list
}

module ExclusivePresence =

    let private present (presence: int list array) = seq {
        for idx in 0..(presence.Length-1) do
            if presence.[idx].Length > 0 then
                yield idx
    }

    let private groupsDownToDepth (depth: int) (presence: int list array) =
        present presence
        |> Seq.toList
        |> MathTools.combinationsDownToDepth depth

    let private commonPlaces (individualPlaces: int list seq) =
        individualPlaces
        |> Seq.fold (fun s p ->
            p
            |> List.fold (fun s p ->
                if s |> List.contains p then s else p :: s
                ) s
            ) []

    let private canEliminateOthers (superRows: int) (superColumns: int) (presence: int list array) (places: int list) (exceptIncluded: int seq -> int seq) =
        let total = superRows * superColumns
        seq { 0..(total-1) }
        |> exceptIncluded
        |> Seq.map (fun other -> presence.[other])
        |> Seq.concat
        |> Seq.distinct
        |> Seq.exists (fun p -> places |> List.contains p)

    let private matchExclussivePresence (superRows: int) (superColumns: int) (depth: int) (mapper: int -> (int * int)) (presence: int list array) = seq {

        for indices in groupsDownToDepth depth presence do
            let places =
                indices
                |> Seq.map (fun x -> presence.[x])
                |> commonPlaces

            if places.Length = indices.Length then

                let exceptIncluded = Seq.filter (fun other -> not (indices |> List.contains other))

                if canEliminateOthers superRows superColumns presence places exceptIncluded
                then yield { Numbers = indices; RowsAndColumns = places |> List.map mapper }

    }

    let findDownToDepth (superRows: int) (superColumns: int) (depth: int) (group: RuleGroup) (opts: Possibilities): ExclusivePresence seq =
        let fields = group |> Seq.toArray
        let values = [| for (row, col) in fields -> opts.[row].[col] |]
        values
        |> Possibilities.toOrderedPresence superRows superColumns
        |> matchExclussivePresence superRows superColumns depth (fun idx -> fields.[idx] )

    let apply (superRows: int) (superColumns: int) (presence: ExclusivePresence) (options: Possibilities) : Possibilities =
        let total = superRows * superColumns
        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if presence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if presence.Numbers |> List.contains num then num ]
                else options.[row].[col]
            |]
        |]
