namespace Ch.PatchCode.SudokuSolver

type SolverState = { Board: Board; Options: Possibilities }

module SolverState =

    let fromBoard (sudoku: Board): SolverState =
        {
            Board = Board.empty sudoku.Length
            Options = Possibilities.fromBoard sudoku
        }

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
