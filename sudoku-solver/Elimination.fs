namespace Ch.PatchCode.SudokuSolver

type EliminationState = {
    Possibilities: Possibilities
    Eliminations: SingularOption list
}

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
                    else
                        [ for x in c do if x <> value.Value then x ]
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

    let eliminate (supRows: int) (supCols: int) (state: EliminationState) : EliminationState =
        match state with
        | { Eliminations = []} -> state
        | {
            Possibilities = possibilities
            Eliminations = value :: remainingEliminations
          } ->
            if not (possibilities.[value.Row].[value.Col] |> List.contains value.Value)
            then
                { Possibilities = possibilities; Eliminations = remainingEliminations }  // not present (anymore) -> do nothing
            else
                let newPossibilities = possibilities |> remove value

                let newEliminations =

                    newPossibilities

                    // if there is only a single option left in the field, then cleanup row, column and block
                    |> fun possibilities ->
                        let append (eliminations: SingularOption list) (elimination: SingularOption) = elimination :: eliminations
                        let merge (eliminations: SingularOption list) (elimination: SingularOption) = append eliminations elimination
                        match possibilities.[value.Row].[value.Col] with
                        | [ single ] ->
                            othersInRowColumnAndBlock supRows supCols { value with Value = single }
                            |> Seq.fold merge remainingEliminations
                        | _ -> remainingEliminations

                    // if this is exclussive, remove others
                    // if absence can be cross-appied, apply

                { Possibilities = newPossibilities; Eliminations = newEliminations }

    let rec meltDown (supRows: int) (supCols: int) (state: EliminationState) : EliminationState =
        match state with
        | { Eliminations = []} -> state
        | someState ->
            eliminate supRows supCols someState
            |> meltDown supRows supCols
