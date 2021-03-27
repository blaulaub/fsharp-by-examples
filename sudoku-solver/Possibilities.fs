namespace Ch.PatchCode.SudokuSolver

type Possibilities = int list array array

module Possibilities =

    // this code is still limited to a 9x9 (3x3) Sudoku
    let private superColumns = 3
    let private superRows = 3

    /// <summary>
    /// Derives initial options from a given <see cref="Board"/> setup.
    /// </summary>
    let fromBoard (board: Board): Possibilities =

        let total = board.Length

        [| for row in board ->
            [| for field in row ->
                match field with
                | Some num -> [ num ]
                | None -> [1..total]
             |]
        |]

    /// <summary>
    /// Reversal: Instead of telling where what is possible, tell what is possible where.
    /// </summary>
    let toOrderedPresence (values: int list array) : int list array =
        let up = superRows * superColumns - 1
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
