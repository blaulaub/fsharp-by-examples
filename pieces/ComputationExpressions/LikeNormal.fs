namespace ChPatchcode.FSharpByExamples.ComputationExpressions

(*
    This Module is in partial response to
    https://fsharpforfunandprofit.com/series/computation-expressions/.

    Here we try to implement a computation expression builder that does just the ordinary,
    so this is named "LikeNormalBuilder" within the module.
*)
module LikeNormal =

    (*
        Regarding custom builder types, see
        https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions#creating-a-new-type-of-computation-expression
    *)
    type LikeNormalBuilder() =
        member _.Bind(x, f) = f x
        member _.Return(x) = x

    let likeNormal = LikeNormalBuilder()
