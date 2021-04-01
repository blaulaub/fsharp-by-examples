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

    let combinationsAtDepth depth totality = seq {

        let rec eval (n: int) (l: 'a list) (gy: 'a list -> 'a list seq) = seq {
            if (n<=0)
            then yield! gy l
            else yield! eval (n-1) l (gx gy)
        }

        yield! eval depth totality (gx g0)
    }

    let combinationsDownToDepth depth totality = seq {

        let rec eval (n: int) (l: 'a list) (gy: 'a list -> 'a list seq) = seq {
            if (n>0) then
                yield! gy l
                yield! eval (n-1) l (gx gy)
        }

        yield! eval depth totality (gx g0)
    }
