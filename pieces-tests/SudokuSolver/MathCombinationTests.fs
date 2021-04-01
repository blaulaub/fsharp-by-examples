module MathCombinationTests

open Expecto
open Ch.PatchCode.SudokuSolver

[<Tests>]
let tests =
    testList "Sudoku MathCombination tests" [

        test "downTo depth zero of empty" {
            Expect.equal (MathCombinations.combinationsDownToDepth 0 [] |> Seq.toList) [] ""
        }

        test "downTo depth zero of one" {
            Expect.equal (MathCombinations.combinationsDownToDepth 0 [1] |> Seq.toList) [] ""
        }

        test "downTo depth one of one" {
            Expect.equal (MathCombinations.combinationsDownToDepth 1 [1] |> Seq.toList) [ [1] ] ""
        }

        test "downTo depth one of two" {
            Expect.equal (MathCombinations.combinationsDownToDepth 1 [1; 2] |> Seq.toList) [ [1]; [2] ] ""
        }

        test "downTo depth two of two" {
            Expect.equal (MathCombinations.combinationsDownToDepth 2 [1; 2] |> Seq.toList) [ [1]; [2]; [1; 2] ] ""
        }

        test "downTo depth two of three" {
            Expect.equal (MathCombinations.combinationsDownToDepth 2 [1; 2; 3] |> Seq.toList) [ [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3] ] ""
        }


        test "at depth zero of empty" {
            Expect.equal (MathCombinations.combinationsAtDepth 0 [] |> Seq.toList) [] ""
        }

        test "at depth zero of one" {
            Expect.equal (MathCombinations.combinationsAtDepth 0 [1] |> Seq.toList) [] ""
        }

        test "at depth one of one" {
            Expect.equal (MathCombinations.combinationsAtDepth 1 [1] |> Seq.toList) [ [1] ] ""
        }

        test "at depth one of two" {
            Expect.equal (MathCombinations.combinationsAtDepth 1 [1; 2] |> Seq.toList) [ [1]; [2] ] ""
        }

        test "at depth two of two" {
            Expect.equal (MathCombinations.combinationsAtDepth 2 [1; 2] |> Seq.toList) [ [1; 2] ] ""
        }

        test "at depth two of three" {
            Expect.equal (MathCombinations.combinationsAtDepth 2 [1; 2; 3] |> Seq.toList) [ [1; 2]; [1; 3]; [2; 3] ] ""
        }
    ]
