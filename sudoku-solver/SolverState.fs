namespace Ch.PatchCode.SudokuSolver

type SolverState = { Board: Board; Options: Possibilities }

module SolverState =

    let fromBoard (sudoku: Board): SolverState =
        {
            Board = Board.empty sudoku.Length
            Options = Possibilities.fromBoard sudoku
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

    let applyExlussivePresenceToState (presence: ExclusivePresence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ExclusivePresence.apply presence
    }

    let applyConclusiveAbsenceToState (absence: ConclusiveAbsence) (oldState: SolverState) = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ConclusiveAbsence.apply absence
    }

    let applyStep (superRows: int) (superColumns: int) (oldState: SolverState) (step: SolutionStep): SolverState =
        match step with
        | ApplySingularOption option ->
            applySingularOptionToState superRows superColumns option oldState
        | ExclusiveInGroup presence ->
            applyExlussivePresenceToState presence oldState
        | AbsentInGroup absence ->
            applyConclusiveAbsenceToState absence oldState

    let singularOptionEliminationSteps options : SolutionStep seq =
        options |> SingularOption.find |> Seq.map ApplySingularOption

    let exclusivePresenceDetectionSteps (superRows: int) (superColumns: int) (depth: int) options : SolutionStep seq = seq {
        let ruleGroups = RuleGroup.groups superRows superColumns
        for group in ruleGroups do
            yield! options |> ExclusivePresence.findDownToDepth depth group |> Seq.map ExclusiveInGroup
    }

    let steps (superRows: int) (superColumns: int) options = seq {
        yield! singularOptionEliminationSteps options

        let ruleGroups = RuleGroup.groups superRows superColumns
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 1 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 2 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 3 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 4 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 5 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 6 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 7 group |> Seq.map ExclusiveInGroup
        for group in ruleGroups do yield! options |> ExclusivePresence.findAtDepth 8 group |> Seq.map ExclusiveInGroup

        let crossGroups = CrossGroup.groupsAtLevel superRows superColumns 1
        for group in crossGroups do yield! options |> ConclusiveAbsence.find group |> Seq.map AbsentInGroup

        ()
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
