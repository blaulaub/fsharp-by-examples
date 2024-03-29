namespace Ch.PatchCode.SudokuSolver

module Utilities =

    let private boardToString<'T> (fieldToString: 'T -> string) (board: 'T array array) =
        board
        |> Seq.map (fun line ->
            line
            |> Seq.map fieldToString
            |> String.concat ""
            )
        |> String.concat "\n"

    /// <summary>
    /// Dump the Sudoku into a nine-line-nine-column string.
    /// </summary>
    let toString = boardToString (fun x ->
        match x with
        | Some value -> sprintf " %2d " value
        | None -> "    "
        )
