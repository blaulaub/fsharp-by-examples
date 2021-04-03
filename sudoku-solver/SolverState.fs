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

    let private applyExlussivePresenceToState (presence: ExclusivePresence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ExclusivePresence.apply presence
    }

    let private applyConclusiveAbsenceToState (absence: ConclusiveAbsence) (oldState: SolverState) = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> ConclusiveAbsence.apply absence
    }

    let private applyStep (superRows: int) (superColumns: int) (oldState: SolverState) (step: SolutionStep): SolverState =
        match step with
        | ApplySingularOption option ->
            applySingularOptionToState superRows superColumns option oldState
        | ExclusiveInGroup presence ->
            applyExlussivePresenceToState presence oldState
        | AbsentInGroup absence ->
            applyConclusiveAbsenceToState absence oldState

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
