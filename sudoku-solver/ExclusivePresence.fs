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

    // this code is still limited to a 9x9 (3x3) Sudoku
    let private superColumns = 3
    let private superRows = 3

    let private commonPlaces (individualPlaces: int list list) =
        individualPlaces
        |> Seq.fold (fun s p ->
            p
            |> List.fold (fun s p ->
                if s |> List.contains p then s else p :: s
                ) s
            ) []

    let canEliminateOthers (presence: int list array) (places: int list) (exceptIncluded: int seq -> int seq) =
        let total = superRows * superColumns
        seq { 0..(total-1) }
        |> exceptIncluded
        |> Seq.map (fun other -> presence.[other])
        |> Seq.concat
        |> Seq.distinct
        |> Seq.exists (fun p -> places |> List.contains p)

    let private matchTwice (mapper: int -> (int * int)) (presence: int list array) = seq {

        let total = superRows * superColumns

        for first in 0..(total-1) do
        if presence.[first].Length > 0 then

            for second in (first+1)..(total-1) do
            if presence.[second].Length > 0 then

                let places =
                    [ presence.[first]; presence.[second] ]
                    |> commonPlaces

                if places.Length = 2 then

                    let exceptIncluded = Seq.filter (fun other -> other <> first && other <> second)

                    if canEliminateOthers presence places exceptIncluded
                    then yield { Numbers = [ first; second ]; RowsAndColumns = places |> List.map mapper }

        for first in 0..(total-1) do
        if presence.[first].Length > 0 then

            for second in (first+1)..(total-1) do
            if presence.[second].Length > 0 then

                for third in (second+1)..(total-1) do
                if presence.[third].Length > 0 then

                    let places =
                        [ presence.[first]; presence.[second]; presence.[third] ]
                        |> commonPlaces

                    if places.Length = 3 then

                        let exceptIncluded = Seq.filter (fun other -> other <> first && other <> second && other <> third)

                        if canEliminateOthers presence places exceptIncluded
                        then yield { Numbers = [ first; second; third ]; RowsAndColumns = places |> List.map mapper }
    }

    let private matchExclussivePresence (mapper: int -> (int * int)) (presence: int list array) = seq {
        let total = superRows * superColumns
        for value in 0..(total-1) do
            match presence.[value] with
            | [ position ] -> yield { Numbers = [value]; RowsAndColumns = [ mapper position] }
            | _ -> ()

        yield! matchTwice mapper presence
    }

    let find (group: RuleGroup) (opts: Possibilities): ExclusivePresence seq =
        let total = superRows * superColumns
        let fields = group |> Seq.toArray
        let values = [| for (row, col) in fields -> opts.[row].[col] |]
        values
        |> Possibilities.toOrderedPresence
        |> matchExclussivePresence (fun idx -> fields.[idx] )

    let apply (presence: ExclusivePresence) (options: Possibilities) : Possibilities =
        let total = superRows * superColumns
        [| for row in 0..(total-1) ->
            [| for col in 0..(total-1) ->
                if presence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if presence.Numbers |> List.contains num then num ]
                else options.[row].[col]
            |]
        |]
