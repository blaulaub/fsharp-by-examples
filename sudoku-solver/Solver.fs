namespace Ch.PatchCode.SudokuSolver

module Solver =

    /// <summary>
    /// Derives initial options from a given <see cref="Sudoku"/> setup.
    /// </summary>
    let possibilities (sudoku: Board): Possibilities =
        [| for row in sudoku ->
            [| for field in row ->
                match field with
                | Some num -> [ num ]
                | None -> [1..9]
             |]
        |]

    /// <summary>
    /// If a particular number cannot be in fields referenced by source,
    /// then it also cannot be in fields referenced by target.
    /// </summary>
    type SingularCrossGroup = {
        Source: (int*int) seq
        Target: (int*int) seq
    }

    let singularCrossGroups (): SingularCrossGroup seq = seq {
        for supCol in 0..2 do
        for row in 0..8 do
        yield {
            Source = seq {
                for col in 0..8 do
                for subRow in 0..2 do
                if row%3 = subRow && col/3 <> supCol then yield (row, col)
            }
            Target = seq {
                for col in 0..8 do
                for subRow in 0..2 do
                if row%3 <> subRow && col/3 = supCol then yield (row, col)
            }
        }
        for supRow in 0..2 do
        for col in 0..8 do
        yield {
            Source = seq {
                for row in 0..8 do
                for subCol in 0..2 do
                if col%3 = subCol && row/3 <> supRow then yield (row, col)
            }
            Target = seq {
                for row in 0..8 do
                for subCol in 0..2 do
                if col%3 <> subCol && row/3 = supRow then yield (row, col)
            }
        }
    }

    /// <summary>
    /// A singular option is a field on the board where
    /// only one value can possibly be the solution.
    /// </summary>
    type SingularOption = { Row: int; Col: int; Value: int }

    /// <summary>
    /// A limited number of values occupies a limited number
    /// of fields, meaning that no other numbers can be on
    /// these fields.
    /// </summary>
    type ExclussivePresence = {
        Numbers: int list
        RowsAndColumns: (int * int) list
    }

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
            Options = possibilities sudoku
        }

    /// <summary>
    /// Union of all considered kinds of solution steps.
    /// </summary>
    type SolutionStep =
    | ApplySingularOption of SingularOption
    | ExclussiveInGroup of ExclussivePresence
    | AbsentInGroup of ConclussiveAbsence

    /// <summary>
    /// Find and return all fields that have a
    /// single possible solution.
    /// </summary>
    let findSingularOptions (opts: Possibilities): SolutionStep seq = seq {
        for row in 0..8 do
        for col in 0..8 do
        match opts.[row].[col] with
        | [ num ] -> yield ApplySingularOption { Row = row; Col = col; Value = num }
        | _ -> ()
    }

    let applySingularOption { Row = targetRow; Col = targetCol; Value = targetNum } (options: Possibilities) : Possibilities =
        [| for row in 0..8 ->
            [| for col in 0..8 ->
                if row = targetRow && col = targetCol
                then
                    []
                else
                    if row <> targetRow && col <> targetCol && (row/3 <> targetRow/3 || col/3 <> targetCol/3)
                    then options.[row].[col]
                    else [ for num in options.[row].[col] do if num <> targetNum then num ]
            |]
        |]

    let applyExclussivePresence (presence: ExclussivePresence) (options: Possibilities) : Possibilities =
        [| for row in 0..8 ->
            [| for col in 0..8 ->
                if presence.RowsAndColumns |> List.contains (row, col)
                then [ for num in options.[row].[col] do if presence.Numbers |> List.contains num then num ]
                else options.[row].[col]
            |]
        |]

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
                |> applySingularOption { Row = targetRow; Col = targetCol; Value = num }
            { Board = newBoard; Options = newOptions }

    let applyExlussivePresenceToState (presence: ExclussivePresence) oldState = {
        Board = oldState.Board  // board is not updated
        Options = oldState.Options |> applyExclussivePresence presence
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
        | ExclussiveInGroup presence ->
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

    let matchTwice (mapper: int -> (int * int)) (presence: int list array) = seq {

        for first in 0..8 do
        if presence.[first].Length > 0 then

            for second in (first+1)..8 do
            if presence.[second].Length > 0 then

                let places =
                    [ presence.[first]; presence.[second] ]
                    |> List.fold (fun s p ->
                        p
                        |> List.fold (fun s p ->
                            if s |> List.contains p then s else p :: s
                            ) s
                        ) []

                if places.Length = 2 then

                    let canEliminateOthers =
                        seq { 0..8 }
                        |> Seq.filter (fun other -> other <> first && other <> second)
                        |> Seq.map (fun other -> presence.[other])
                        |> Seq.concat
                        |> Seq.distinct
                        |> Seq.exists (fun p -> places |> List.contains p)

                    if canEliminateOthers then yield ExclussiveInGroup { Numbers = [ first+1; second+1 ]; RowsAndColumns = places |> List.map mapper }


        for first in 0..8 do
        if presence.[first].Length > 0 then

            for second in (first+1)..8 do
            if presence.[second].Length > 0 then

                for third in (second+1)..8 do
                if presence.[third].Length > 0 then

                    let places =
                        [ presence.[first]; presence.[second]; presence.[third] ]
                        |> List.fold (fun s p ->
                            p
                            |> List.fold (fun s p ->
                                if s |> List.contains p then s else p :: s
                                ) s
                            ) []

                    if places.Length = 3 then

                        let canEliminateOthers =
                            seq { 0..8 }
                            |> Seq.filter (fun other -> other <> first && other <> second && other <> third)
                            |> Seq.map (fun other -> presence.[other])
                            |> Seq.concat
                            |> Seq.distinct
                            |> Seq.exists (fun p -> places |> List.contains p)

                        if canEliminateOthers then yield ExclussiveInGroup { Numbers = [ first+1; second+1; third+1 ]; RowsAndColumns = places |> List.map mapper }
    }

    let matchExclussivePresence (mapper: int -> (int * int)) (presence: int list array) = seq {
        for value in 0..8 do
            match presence.[value] with
            | [ position ] -> yield ExclussiveInGroup { Numbers = [value+1]; RowsAndColumns = [ mapper position] }
            | _ -> ()

        yield! matchTwice mapper presence
    }

    let analyse (opts: Possibilities) (group: RuleGroup) : SolutionStep seq =
        let fields = group |> Seq.toArray
        let values = [| for (row, col) in fields -> opts.[row].[col] |]
        values
        |> toOrderedPresence
        |> matchExclussivePresence (fun idx -> fields.[idx] )

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
        for group in singularCrossGroups() do
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