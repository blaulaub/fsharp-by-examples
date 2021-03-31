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

    let present upper lower (presence: int list array) = seq { for idx in lower..upper do if presence.[idx].Length > 0 then yield idx }

    let groups (presence: int list array) (total: int) (depth: int) =
        let rec _groups lower upper (presence: int list array) (soFar: int list) n = seq {
            if n > 0 then
                for first in (presence |> present upper lower) do
                    yield first :: soFar

                for first in (presence |> present upper lower) do
                    yield! _groups (first+1) upper presence (first :: soFar) (n-1)
        }
        _groups 0 (total-1) presence [] depth

    let g (presence: int list array) (total: int) (depth: int) = seq {
        let l = present (total-1) 0 presence |> Seq.toList

        let g0 (l: int list) = seq {
            yield []
        }

        let rec g1 (l: int list) = seq {
            match l with
            | [] -> ()
            | f :: r ->
                for b in g0 r do
                yield f :: b
                yield! g1 r
        }

        let rec g2 (l: int list) = seq {
            match l with
            | [] -> ()
            | f :: r ->
                for b in g1 r do
                    yield f :: b
                yield! g2 r
        }

        let rec gx g1 (l: int list) = seq {
            match l with
            | [] -> ()
            | f :: r ->
                for b in g1 r do
                    yield f :: b
                yield! gx g1 r
        }

        yield! gx g0 l
        yield! gx (gx g0) l
        yield! gx (gx (gx g0)) l
    }



    let private commonPlaces (individualPlaces: int list seq) =
        individualPlaces
        |> Seq.fold (fun s p ->
            p
            |> List.fold (fun s p ->
                if s |> List.contains p then s else p :: s
                ) s
            ) []

    let private canEliminateOthers (presence: int list array) (places: int list) (exceptIncluded: int seq -> int seq) =
        let total = superRows * superColumns
        seq { 0..(total-1) }
        |> exceptIncluded
        |> Seq.map (fun other -> presence.[other])
        |> Seq.concat
        |> Seq.distinct
        |> Seq.exists (fun p -> places |> List.contains p)

    let private matchExclussivePresence (mapper: int -> (int * int)) (presence: int list array) = seq {

        let total = superRows * superColumns

        for indices in groups presence total 3 do
            let places =
                indices
                |> Seq.map (fun x -> presence.[x])
                |> commonPlaces

            if places.Length = indices.Length then

                let exceptIncluded = Seq.filter (fun other -> not (indices |> List.contains other))

                if canEliminateOthers presence places exceptIncluded
                then yield { Numbers = indices; RowsAndColumns = places |> List.map mapper }

    }

    let find (group: RuleGroup) (opts: Possibilities): ExclusivePresence seq =
        let fields = group |> Seq.toArray
        let values = [| for (row, col) in fields -> opts.[row].[col] |]
        values
        |> Possibilities.toOrderedPresence superRows superColumns
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
