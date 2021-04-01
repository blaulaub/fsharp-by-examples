namespace Ch.PatchCode.SudokuSolver

module MathTools =

    let combinationsDownToDepth maxCount totality = seq {

        let g0 (l: 'a list) = seq { yield [] }

        let rec gx (g1: 'a list -> 'a list seq) (l: 'a list) = seq {
            match l with
            | [] -> ()
            | f :: r ->
                for b in g1 r do
                    yield f :: b
                yield! gx g1 r
        }

        let rec eval (gy: 'a list -> 'a list seq) (l: 'a list) (n: int) = seq {
            if (n>0) then
                yield! gy l
                yield! eval (gx gy) l (n-1)
        }

        yield! eval (gx g0) totality maxCount
    }
