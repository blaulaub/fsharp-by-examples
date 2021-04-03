namespace Ch.PatchCode.SudokuSolver

module RandomInventor =

    let private apply (superRows: int) (superColumns: int) next0 (row, col, n) =
        next0
        |> SolverState.applySingularOptionToState superRows superColumns { Row = row; Col = col; Value = n}


    let private nextStateAndPick (superRows: int) (superColumns: int) (naturalBelow: int -> int) state =

        let total = superRows * superColumns

        let next0 =
            state
            |> SolverState.solve superRows superColumns

        let undet = [|
            for row in 0..(total-1) do
            for col in 0..(total-1) do
            if next0.Options.[row].[col].Length > 0
            then yield (row, col)
        |]

        if undet.Length > 0
        then
            let (row, col) = undet.[naturalBelow undet.Length]
            let l = naturalBelow next0.Options.[row].[col].Length
            let n = next0.Options.[row].[col].[l]
            (next0, Some (row, col, n))
        else
            (next0, None)

    let rec private getHints (superRows: int) (superColumns: int) (naturalBelow: int -> int) state = seq {
        match nextStateAndPick superRows superColumns naturalBelow state with
        | (next0, Some pick) ->
            yield pick
            yield! getHints superRows superColumns naturalBelow (apply superRows superColumns next0 pick)
        | (_, None) -> ()
    }

    /// <summary>
    /// Invent some possibly unsolvable Sudoku puzzle.
    /// </summary>
    /// <remarks>
    /// Depending on whether the given <c>naturalBelow</c> is deterministic or
    /// random, the solution will be deterministic or random. Depending on the
    /// strength of the solver, it may come up with
    /// unsolvable (self-contradictory) Sudokus.
    /// </remarks>
    /// <param name="superRows">Number of block rows.</param>
    /// <param name="superColumns">Number of block columns.</param>
    /// <param name="naturalBelow">Produces an integer between zero (inclusive) and the given input (exclusive).</param>
    let invent (superRows: int) (superColumns: int) (naturalBelow: int -> int) : Board =

        let initialState () = Board.empty (superRows*superColumns) |> SolverState.fromBoard

        initialState ()
        |> getHints superRows superColumns naturalBelow
        |> Seq.fold (apply superRows superColumns) (initialState ())
        |> (fun state -> state.Board)
