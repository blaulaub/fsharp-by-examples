namespace Ch.PatchCode.SudokuSolver

module Solver =

    /// <summary>
    /// Union of all considered kinds of solution steps.
    /// </summary>
    type SolutionStep =
    | ApplySingularOption of SingularOption
    | ExclusiveInGroup of ExclusivePresence
    | AbsentInGroup of ConclusiveAbsence

    let private applyStep (superRows: int) (superColumns: int) (oldState: SolverState) (step: SolutionStep): SolverState =
        match step with
        | ApplySingularOption option ->
            SolverState.applySingularOptionToState superRows superColumns option oldState
        | ExclusiveInGroup presence ->
            SolverState.applyExlussivePresenceToState presence oldState
        | AbsentInGroup absence ->
            SolverState.applyConclusiveAbsenceToState absence oldState

    let private steps (superRows: int) (superColumns: int) (possibilities: Possibilities) : SolutionStep seq = seq {

        yield!
            possibilities
            |> SingularOption.find
            |> Seq.map ApplySingularOption

        let total = superRows * superColumns

        let ruleGroups = RuleGroup.groups superRows superColumns
        for depth in 1..(total-1) do
            for group in ruleGroups do
                yield!
                    possibilities
                    |> ExclusivePresence.findAtDepth depth group
                    |> Seq.map ExclusiveInGroup

        let min a b = if a < b then a else b
        for depth in 1..((min superRows superColumns)/2) do
            let crossGroups = CrossGroup.groupsAtLevel superRows superColumns depth
            for group in crossGroups do
                yield!
                    possibilities
                    |> ConclusiveAbsence.find group
                    |> Seq.map AbsentInGroup

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
