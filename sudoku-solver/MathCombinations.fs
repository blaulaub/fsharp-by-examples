namespace Ch.PatchCode.SudokuSolver

module MathCombinations =

    let private g0 (l: 'a list) = seq { yield [] }

    let rec private gx (g1: 'a list -> 'a list seq) (l: 'a list) = seq {
        match l with
        | [] -> ()
        | f :: r ->
            for b in g1 r do
                yield f :: b
            yield! gx g1 r
    }

    let rec private evalAt (n: int) (l: 'a list) (gy: 'a list -> 'a list seq) = seq {
        if (n=1) then yield! gy l
        if (n>1) then yield! evalAt (n-1) l (gx gy)
    }

    let combinationsAtDepth depth totality = evalAt depth totality (gx g0)

    let rec private evalDownTo (n: int) (l: 'a list) (gy: 'a list -> 'a list seq) = seq {
        if (n>0) then
            yield! gy l
            yield! evalDownTo (n-1) l (gx gy)
    }

    let combinationsDownToDepth depth totality = evalDownTo depth totality (gx g0)
