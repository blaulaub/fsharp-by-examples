namespace Ch.PatchCode.SudokuSolver

type SolverState = { Board: Board; Options: Possibilities }

module SolverState =

    let fromBoard (superRows: int) (superColumns: int) (sudoku: Board): SolverState =
        {
            Board = Board.empty superRows superColumns
            Options = Possibilities.fromBoard superRows superColumns sudoku
        }

    /// <summary>
    /// Union of all considered kinds of solution steps.
    /// </summary>
    type SolutionStep =
    | ApplySingularOption of SingularOption
    | ExclusiveInGroup of ExclusivePresence
    | AbsentInGroup of ConclusiveAbsence

    let applySingularOptionToState (superRows: int) (superColumns: int) { Row = targetRow; Col = targetCol; Value = num } oldState =
        let total = superRows * superColumns
        let oldBoard = oldState.Board
        let newBoard =
            [| for row in 0..(total-1) ->
                if row <> targetRow
                then oldBoard.[row]
                else
                    [| for col in 0..(total-1) ->
                        if col <> targetCol
                        then oldBoard.[row].[col]
                        else Some (num+1)
                    |]
            |]
        let oldOptions = oldState.Options
        let newOptions =
            oldOptions
            |> SingularOption.apply superRows superColumns { Row = targetRow; Col = targetCol; Value = num }
        { Board = newBoard; Options = newOptions }

    let applyExlussivePresenceToState (superRows: int) (superColumns: int) (presence: ExclusivePresence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ExclusivePresence.apply superRows superColumns presence
    }

    let applyConclusiveAbsenceToState (superRows: int) (superColumns: int) (absence: ConclusiveAbsence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ConclusiveAbsence.apply superRows superColumns absence
    }

    let applyStep (superRows: int) (superColumns: int) (oldState: SolverState) (step: SolutionStep): SolverState =
        match step with
        | ApplySingularOption option ->
            applySingularOptionToState superRows superColumns option oldState
        | ExclusiveInGroup presence ->
            applyExlussivePresenceToState superRows superColumns presence oldState
        | AbsentInGroup absence ->
            applyConclusiveAbsenceToState superRows superColumns absence oldState

    let singularOptionEliminationSteps (superRows: int) (superColumns: int) options : SolutionStep seq =
        options |> SingularOption.find superRows superColumns |> Seq.map ApplySingularOption

    let exclusivePresenceDetectionSteps (superRows: int) (superColumns: int) (depth: int) options : SolutionStep seq = seq {
        for group in RuleGroup.groups superRows superColumns do
            yield! options |> ExclusivePresence.findDownToDepth superRows superColumns depth group |> Seq.map ExclusiveInGroup
    }

    let conclusiveAbsenceDetectionSteps (superRows: int) (superColumns: int) (depth: int) options : SolutionStep seq = seq {
        for group in CrossGroup.singularCrossGroups superRows superColumns do
            yield! options |> ConclusiveAbsence.findDownToDepth depth group |> Seq.map AbsentInGroup
    }

    let steps (superRows: int) (superColumns: int) options = seq {
        yield! singularOptionEliminationSteps superRows superColumns options
        yield! exclusivePresenceDetectionSteps superRows superColumns 1 options
        yield! conclusiveAbsenceDetectionSteps superRows superColumns 1 options
    }

    let rec solveWithPreAction (superRows: int) (superColumns: int) preAction state =
        preAction state
        let next =
            steps superRows superColumns state.Options
            |> Seq.tryHead
            |> Option.map (applyStep superRows superColumns state)
        match next with
        | Some newState -> solveWithPreAction superRows superColumns preAction newState
        | None -> state

    let solve (superRows: int) (superColumns: int) state = solveWithPreAction superRows superColumns (ignore) state
