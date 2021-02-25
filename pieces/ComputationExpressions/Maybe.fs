namespace ChPatchcode.FSharpByExamples.ComputationExpressions

module Maybe =

    type MaybeBuilder() =
        member _.Bind(x, f) =
            match x with
            | None -> None
            | Some a -> f a

        member _.Return(x) =
            Some x

    let maybe = MaybeBuilder()
