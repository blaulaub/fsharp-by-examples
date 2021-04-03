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

    let exclusivePresenceIterator (superRows: int) (superColumns: int) (maxDepth: int) = seq {
        let ruleGroups = RuleGroup.groups superRows superColumns
        for depth in 1..maxDepth do
            for group in ruleGroups do
                yield (depth, group)
    }

    let findExclusiveInGroup (possibilities: Possibilities) (input: (int*RuleGroup)seq) = async {
        return
            input
            |> Seq.map (fun (depth, group) ->
                possibilities
                |> ExclusivePresence.findAtDepth depth group
                |> Seq.map ExclusiveInGroup
                |> Seq.tryHead
                )
            |> Seq.filter Option.isSome
            |> Seq.tryHead
            |> Option.flatten
    }

    let conclusiveAbsenceIterator (superRows: int) (superColumns: int) = seq {
        let min a b = if a < b then a else b
        for depth in 1..((min superRows superColumns)/2) do
            let crossGroups = CrossGroup.groupsAtLevel superRows superColumns depth
            for group in crossGroups do
                yield group
    }

    let findAbsentInGroup (possibilities: Possibilities) (input: CrossGroup seq) = async {
        return
            input
            |> Seq.map (fun group ->
                possibilities
                |> ConclusiveAbsence.find group
                |> Seq.map AbsentInGroup
                |> Seq.tryHead
                )
            |> Seq.filter Option.isSome
            |> Seq.tryHead
            |> Option.flatten
    }

    let split (groups: int) (input: 'a seq): 'a list array =
        input
        |> Seq.fold (fun (c, l) v -> ((c+1)%groups, (c, v) :: l)) (0, [])
        |> snd
        |> Seq.fold (fun (s: 'a list array) (c, v) -> s.[c] <- v :: s.[c]; s) [| for _ in 1..groups -> [] |]

    let private steps (superRows: int) (superColumns: int) (possibilities: Possibilities) : SolutionStep seq = seq {

        let cores = System.Environment.ProcessorCount

        yield!
            possibilities
            |> SingularOption.find
            |> Seq.map ApplySingularOption

        let maxDepth = superRows * superColumns - 1
        let iters1 = exclusivePresenceIterator superRows superColumns maxDepth |> split cores
        let actions1 = iters1 |> Array.map (findExclusiveInGroup possibilities)
        let res1 = actions1 |> Async.Parallel |> Async.RunSynchronously
        for res in res1 do
            match res with
            | Some result -> yield result
            | None -> ()

        let iters2 = conclusiveAbsenceIterator superRows superColumns |> split cores
        let actions2 = iters2 |> Array.map (findAbsentInGroup possibilities)
        let res2 = actions2 |> Async.Parallel |> Async.RunSynchronously
        for res in res2 do
            match res with
            | Some result -> yield result
            | None -> ()

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
