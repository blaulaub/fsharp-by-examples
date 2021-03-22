module SudokuSolver

// a board with actually known field values
type KnownValues = int option array array

// actions on a board
type UpdateKnownValues = { Row: int; Col: int; Value: int }

// a board with remaing Possibilities
type Possibilities = int list array array

// actions on a board of possibilities
type RemovePossibilityFromRow = { Row: int; Value: int }
type RemovePossibilityFromCol = { Col: int; Value: int }
type RemovePossibilityFromSubBlock = { SupRow: int; SupCol: int; Value: int }

// (and the discriminated union thereof)
type PossibilityReduction =
| RemoveFromRow of RemovePossibilityFromRow
| RemoveFromCol of RemovePossibilityFromCol
| RemoveFromSubBlock of RemovePossibilityFromSubBlock

type SolutionState = {
    Board: KnownValues
    Possibilities: Possibilities
}

// a solution update for both
type SolutionUpdate = {
    Update: UpdateKnownValues
    Reductions: PossibilityReduction list
}

// Regular niner subsets of possibility
type NinerRowGroup = { Row: int; Values: int list array }
type NinerColGroup = { Col: int; Values: int list array }
type NinerSubBlock = { SubRow: int; SubCol: int; Values: int list array }

type NinerGroup =
| NinerRowGroup of NinerRowGroup
| NinerColGroup of NinerColGroup
| NinerSubBlock of NinerSubBlock
