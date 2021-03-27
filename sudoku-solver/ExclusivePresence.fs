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

    let private toOrderedPresence (values: int list array) : int list array =
        let up = superRows * superColumns - 1
        values
        |> Array.rev
        |> Array.fold (fun (i, state) l ->
            (
                i-1,
                l |> List.fold (fun state v ->
                    // note: we may create a lots of arrays here, maybe that is not cheap...
                    [|
                        for idx in 0..up ->
                            if idx = (v-1)
                            then i::state.[idx]
                            else state.[idx]
                    |]
                ) state
            )
        ) (up, [| for _ in 0..up -> [] |])
        |> snd

    let matchTwice (mapper: int -> (int * int)) (presence: int list array) = seq {

        for first in 0..8 do
        if presence.[first].Length > 0 then

            for second in (first+1)..8 do
            if presence.[second].Length > 0 then

                let places =
                    [ presence.[first]; presence.[second] ]
                    |> List.fold (fun s p ->
                        p
                        |> List.fold (fun s p ->
                            if s |> List.contains p then s else p :: s
                            ) s
                        ) []

                if places.Length = 2 then

                    let canEliminateOthers =
                        seq { 0..8 }
                        |> Seq.filter (fun other -> other <> first && other <> second)
                        |> Seq.map (fun other -> presence.[other])
                        |> Seq.concat
                        |> Seq.distinct
                        |> Seq.exists (fun p -> places |> List.contains p)

                    if canEliminateOthers then yield { Numbers = [ first+1; second+1 ]; RowsAndColumns = places |> List.map mapper }


        for first in 0..8 do
        if presence.[first].Length > 0 then

            for second in (first+1)..8 do
            if presence.[second].Length > 0 then

                for third in (second+1)..8 do
                if presence.[third].Length > 0 then

                    let places =
                        [ presence.[first]; presence.[second]; presence.[third] ]
                        |> List.fold (fun s p ->
                            p
                            |> List.fold (fun s p ->
                                if s |> List.contains p then s else p :: s
                                ) s
                            ) []

                    if places.Length = 3 then

                        let canEliminateOthers =
                            seq { 0..8 }
                            |> Seq.filter (fun other -> other <> first && other <> second && other <> third)
                            |> Seq.map (fun other -> presence.[other])
                            |> Seq.concat
                            |> Seq.distinct
                            |> Seq.exists (fun p -> places |> List.contains p)

                        if canEliminateOthers then yield { Numbers = [ first+1; second+1; third+1 ]; RowsAndColumns = places |> List.map mapper }
    }

    let matchExclussivePresence (mapper: int -> (int * int)) (presence: int list array) = seq {
        for value in 0..8 do
            match presence.[value] with
            | [ position ] -> yield { Numbers = [value+1]; RowsAndColumns = [ mapper position] }
            | _ -> ()

        yield! matchTwice mapper presence
    }

    let find (opts: Possibilities) (group: RuleGroup): ExclusivePresence seq =
        let total = superRows * superColumns
        let fields = group |> Seq.toArray
        let values = [| for (row, col) in fields -> opts.[row].[col] |]
        values
        |> toOrderedPresence
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
