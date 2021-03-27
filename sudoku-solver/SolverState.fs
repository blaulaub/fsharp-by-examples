namespace Ch.PatchCode.SudokuSolver

type SolverState = { Board: Board; Options: Possibilities }

module SolverState =

    let fromBoard (sudoku: Board): SolverState =
        {
            Board = Board.empty 3 3
            Options = Possibilities.fromBoard sudoku
        }

    /// <summary>
    /// Union of all considered kinds of solution steps.
    /// </summary>
    type SolutionStep =
    | ApplySingularOption of SingularOption
    | ExclusiveInGroup of ExclusivePresence
    | AbsentInGroup of ConclusiveAbsence

    let applySingularOptionToState { Row = targetRow; Col = targetCol; Value = num } oldState =
        let oldBoard = oldState.Board
        let newBoard =
            [| for row in 0..8 ->
                if row <> targetRow
                then oldBoard.[row]
                else
                    [| for col in 0..8 ->
                        if col <> targetCol
                        then oldBoard.[row].[col]
                        else Some num
                    |]
            |]
        let oldOptions = oldState.Options
        let newOptions =
            oldOptions
            |> SingularOption.apply { Row = targetRow; Col = targetCol; Value = num }
        { Board = newBoard; Options = newOptions }

    let applyExlussivePresenceToState (presence: ExclusivePresence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ExclusivePresence.apply presence
    }

    let applyConclusiveAbsenceToState (absence: ConclusiveAbsence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ConclusiveAbsence.apply absence
    }

    let applyStep (oldState: SolverState) (step: SolutionStep): SolverState =
        match step with
        | ApplySingularOption option ->
            applySingularOptionToState option oldState
        | ExclusiveInGroup presence ->
            applyExlussivePresenceToState presence oldState
        | AbsentInGroup absence ->
            applyConclusiveAbsenceToState absence oldState

    let steps options = seq {
        // first try eliminating singular options
        yield! options |> SingularOption.find |> Seq.map ApplySingularOption
        // next try checking niner groups
        for group in RuleGroup.groups 3 3 do
            yield! options |> ExclusivePresence.find group |> Seq.map ExclusiveInGroup
        // next try checking cross groups
        for group in CrossGroup.singularCrossGroups() do
            yield! options |> ConclusiveAbsence.find group |> Seq.map AbsentInGroup
    }

    let rec solveWithPreAction preAction state =
        preAction state
        let next =
            steps state.Options
            |> Seq.tryHead
            |> Option.map (applyStep state)
        match next with
        | Some newState -> solveWithPreAction preAction newState
        | None -> state

    let solve state = solveWithPreAction (ignore) state
