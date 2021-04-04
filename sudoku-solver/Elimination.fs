namespace Ch.PatchCode.SudokuSolver

module Elimination =

    let private remove (value: SingularOption) (possibilities: Possibilities) =
        [| for row in 0..(possibilities.Length-1) ->
            let r = possibilities.[row]
            if (row <> value.Row)
            then r
            else
                [| for col in 0..(r.Length-1) ->
                    let c = r.[col]
                    if (col <> value.Col)
                    then c
                    else [ for x in c do if x <> value.Value then x ]
                |]
        |]

    let private othersInRowColumnAndBlock (supRows: int) (supCols: int) (value: SingularOption) = seq {
        let total = supRows * supCols
        for x in 0..(total-1) do
            if x <> value.Row then yield { value with Row = x }
            if x <> value.Col then yield { value with Col = x }
        let R = (value.Row/supRows)*supRows
        let C = (value.Col/supCols)*supCols
        for r in 0..(supCols-1) do
            for c in 0..(supRows-1) do
                if R+r <> value.Row && C+c <> value.Col then yield { value with Row = R+r; Col = C+c }
    }

    let rec eliminate (supRows: int) (supCols: int) (possibilities: Possibilities) (value: SingularOption) : Possibilities =
        if not (possibilities.[value.Row].[value.Col] |> List.contains value.Value)
        then
            possibilities  // not present -> do nothing
        else
            possibilities

            // remove the value from its field
            |> remove value

            // if there is only a single option left in the field, then cleanup row, column and block
            |> fun possibilities ->
                match possibilities.[value.Row].[value.Col] with
                | [ single ] ->
                    othersInRowColumnAndBlock supRows supCols { value with Value = single }
                    |> Seq.fold (fun possibilities -> eliminate supRows supCols possibilities) possibilities
                | _ -> possibilities

            // if this is exclussive, remove others
            // if absence can be cross-appied, apply
