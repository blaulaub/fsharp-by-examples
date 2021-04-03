namespace Ch.PatchCode.SudokuSolver

module RandomInventor =

    let private pickOneRemainingPossibility (naturalBelow: int -> int) (possibilities: Possibilities) : SingularOption option =

        let undet = [|
            for row in 0..(possibilities.Length-1) do
            for col in 0..(possibilities.[row].Length-1) do
            if possibilities.[row].[col].Length > 0
            then yield (row, col)
        |]

        if undet.Length > 0
        then
            let (row, col) = undet.[naturalBelow undet.Length]
            let l = naturalBelow possibilities.[row].[col].Length
            let n = possibilities.[row].[col].[l]
            Some { Row = row; Col = col; Value = n}
        else
            None

    let private nextStateAndPick (superRows: int) (superColumns: int) (naturalBelow: int -> int) state =

        let nextState =
            state
            |> Solver.solve superRows superColumns

        (nextState, nextState.Options |> pickOneRemainingPossibility naturalBelow)

    let private apply (superRows: int) (superColumns: int) state singularOption =
        state |> SolverState.applySingularOptionToState superRows superColumns singularOption

    let rec private getHints (superRows: int) (superColumns: int) (naturalBelow: int -> int) state = seq {
        match nextStateAndPick superRows superColumns naturalBelow state with
        | (solutionSoFar, Some nextHint) ->
            yield nextHint
            yield!
                solutionSoFar
                |> SolverState.applySingularOptionToState superRows superColumns nextHint
                |> getHints superRows superColumns naturalBelow
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
