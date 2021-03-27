namespace Ch.PatchCode.SudokuSolver

module Solver =

    /// <summary>
    /// A list of values that are definetly not present in a list of fields.
    /// </summary>
    type ConclussiveAbsence = {
        Numbers: int list
        RowsAndColumns: (int * int) list
    }

    type SolutionState = { Board: Board; Options: Possibilities }

    let initialSolutionState (sudoku: Board): SolutionState =
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
    | AbsentInGroup of ConclussiveAbsence

    let findSingularOptions (opts: Possibilities): SolutionStep seq =
        opts |> SingularOption.find |> Seq.map ApplySingularOption

    let applyConclusiveAbsence (absence: ConclussiveAbsence) (options: Possibilities) : Possibilities =
        [| for row in 0..8 ->
            [| for col in 0..8 ->
                if absence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if not (absence.Numbers |> List.contains num) then num ]
                else options.[row].[col]
            |]
        |]

    let applySingularOptionToState { Row = targetRow; Col = targetCol; Value = num } oldState =
            printfn "field row %d col %d has single option %d" targetRow targetCol num
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

    let applyConclusiveAbsenceToState (absence: ConclussiveAbsence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> applyConclusiveAbsence absence
    }

    let applyStep (oldState: SolutionState) (step: SolutionStep): SolutionState =
        match step with
        | ApplySingularOption option ->
            printfn "(singular %d at %dx%d)" option.Value (option.Row+1) (option.Col+1)
            applySingularOptionToState option oldState
        | ExclusiveInGroup presence ->
            printfn "(exclussive values %A at %A)" presence.Numbers presence.RowsAndColumns
            applyExlussivePresenceToState presence oldState
        | AbsentInGroup absence ->
            printfn "(absent values %A at %A)" absence.Numbers absence.RowsAndColumns
            applyConclusiveAbsenceToState absence oldState

    let toOrderedPresence (values: int list array) : int list array =
        let up = values.Length - 1
        values
        |> Array.rev
        |> Array.fold (fun (i, state) l ->
            (
                i-1,
                l |> List.fold (fun state v ->
                    // note: we may create a lots of arrays here, maybe that is not cheap...
                    [|
                        for idx in 0..up ->
                            if idx = (v-1)
                            then i::state.[idx]
                            else state.[idx]
                    |]
                ) state
            )
        ) (up, [| for _ in 0..up -> [] |])
        |> snd

    let mapFromIndexInRow row idx = (row, idx)
    let mapFromIndexInColumn col idx = (idx, col)
    let mapFromIndexInBlock block idx = ((block/3)*3+idx/3,(block%3)*3+idx%3)

    let analyse (opts: Possibilities) (group: RuleGroup) : SolutionStep seq =
        ExclusivePresence.find opts group
        |> Seq.map ExclusiveInGroup

    let analyseCross (opts: Possibilities) (group: SingularCrossGroup) : SolutionStep seq =
        let source = group.Source |> Seq.toArray
        let target = group.Target |> Seq.toArray

        let findPossibilities target =
            target
            |> Seq.map (fun (row, col) -> opts.[row].[col])
            |> Seq.concat
            |> Seq.distinct
            |> Seq.toList

        let sourcePossibilities () = source |> findPossibilities

        findPossibilities target
        |> Seq.filter (fun inTarget -> sourcePossibilities() |> List.contains inTarget )
        |> Seq.map (fun value -> AbsentInGroup { Numbers = [value]; RowsAndColumns = source |> Seq.toList })

    let steps options = seq {
        // first try eliminating singular options
        yield! findSingularOptions options
        // next try checking niner groups
        for group in RuleGroup.groups 3 3 do
            yield! analyse options group
        // next try checking cross groups
        for group in CrossGroup.singularCrossGroups() do
            yield! analyseCross options group
    }

    let rec solveState state preAction =
        preAction state
        let next =
            steps state.Options
            |> Seq.tryHead
            |> Option.map (applyStep state)
        match next with
        | Some newState -> solveState newState preAction
        | None -> state
