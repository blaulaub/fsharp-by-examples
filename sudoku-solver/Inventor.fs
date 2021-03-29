namespace Ch.PatchCode.SudokuSolver

module Inventor =

    let private apply next0 (row, col, n) =
        next0
        |> SolverState.applySingularOptionToState { Row = row; Col = col; Value = n}


    let private nextStateAndPick (superRows: int) (superColumns: int) (rnd: int -> int) state =

        let total = superRows * superColumns

        let next0 =
            state
            |> SolverState.solve

        let undet = [|
            for row in 0..(total-1) do
            for col in 0..(total-1) do
            if next0.Options.[row].[col].Length > 0
            then yield (row, col)
        |]

        if undet.Length > 0
        then
            let (row, col) = undet.[rnd undet.Length]
            let l = rnd next0.Options.[row].[col].Length
            let n = next0.Options.[row].[col].[l]
            (next0, Some (row, col, n))
        else
            (next0, None)

    let rec private getHints (superRows: int) (superColumns: int) (rnd: int -> int) state = seq {
        match nextStateAndPick superRows superColumns rnd state with
        | (next0, Some pick) ->
            yield pick
            yield! getHints superRows superColumns rnd (apply next0 pick)
        | (_, None) -> ()
    }

    let invent (superRows: int) (superColumns: int) (nextRandom: int -> int) =

        let initialState () = Board.empty superRows superColumns |> SolverState.fromBoard

        initialState ()
        |> getHints superRows superColumns nextRandom
        |> Seq.fold apply (initialState ())
        |> (fun state -> state.Board)
