namespace ChPatchcode.FSharpByExamples.ComputationExpressions

(*
    This Module is in partial response to
    https://fsharpforfunandprofit.com/series/computation-expressions/.

    Here we try to implement a computation expression builder that does just the ordinary,
    so this is named "LikeNormalBuilder" within the module.
*)
module LikeNormal =

    (* Below is the definition of the builder*)

    type LikeNormalBuilder() =
        // QUESTION when declaring class members in F#, should the prefix be "this", "_", or just anything?
        member _.Bind(x, f) = f x
        member _.Return(x) = x

    // QUESTION "Fun & Profit" often uses the "new" keyword, is this now obsolete or optional?
    // ANSWER https://stackoverflow.com/questions/4520640/f-new-keyword-what-is-it-for
    let likeNormal = LikeNormalBuilder()


    (* anything below is some code to exercise the builder *)

    let private normalWorkflow = likeNormal {
        // NOTE the compiler translates "let!" to LikeNormalBuilder.Bind
        let! x = 42
        let! y = 43
        let! z = x + y
        // NOTE the compiler translates "return" to LikeNormalBuilder.Return
        return z
    }

    // this is like a self-check at the end of a module-open
    // QUESTION is it ok to have (pure) sanity checks at the end of a module (replacing or at least forestalling any unit tests)?
    // QUESTION if a module has any sanity checks at the end, can they be made impure (printing, logging, reporting) in an acceptable way?
    if normalWorkflow <> 85 then failwith "normalWorkflow should return 85"
