namespace Ch.PatchCode.SudokuSolver

type Possibilities = int list array array

module Possibilities =

    /// <summary>
    /// Derives initial options from a given <see cref="Board"/> setup.
    /// </summary>
    let fromBoard (board: Board): Possibilities =

        let initialGuess = [0..(board.Length-1)]

        [| for row in board ->
            [| for field in row ->
                match field with
                | Some num -> [ num-1 ]
                | None -> initialGuess
             |]
        |]

    /// <summary>
    /// Reversal: Instead of telling where what is possible, tell what is possible where.
    /// </summary>
    let toOrderedPresence (total: int) (values: int list array) : int list array =

        let initialState = ((total-1), [| for _ in 0..(total-1) -> [] |])

        values
        |> Array.rev
        |> Array.fold (fun (i, state) l ->
            (
                i-1,
                l |> List.fold (fun state v ->
                    // note: we may create a lots of arrays here, maybe that is not cheap...
                    [|
                        for idx in 0..(total-1) ->
                            if idx = v
                            then i::state.[idx]
                            else state.[idx]
                    |]
                ) state
            )
        ) initialState
        |> snd
