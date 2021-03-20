// inspired by https://www.youtube.com/watch?v=XrNdvWqxBvA


// BOOLEANS


// what does a boolean do?
// a boolean makes a choice
let True<'a>  (x:'a) (y:'a) = x
let False<'a> (x:'a) (y:'a) = y

// we can define if-then-else as follows
let IfTe bool t e = bool t e


// NON-NEGATIVE INTEGERS


// what does an integer do?
// an integer counts loop iterations
do    // just an illustration, 'do ... ()' ensures it will have no effect
    let two  iter x = iter (iter x)
    let one  iter x = iter x
    let zero iter x = x
    ()

// with explicit typing, to ensure uniform type signature
let zero<'a> (incr:'a->'a) (neutral:'a) :'a = neutral
let one<'a>  (incr:'a->'a) (neutral:'a) :'a = incr neutral

// we can evaluate the integer
// (given an inc-by-one as iter function and 0 as input value)
one ((+)1) 0

// we can compute sums
do    // just an illustration, 'do ... ()' ensures it will have no effect
    let sum a b iter x = b iter (a iter x)
    ()

// with explicit typing for uniform type signatures
let sum<'a> (a:('a ->'a)->'a->'a) (b:('a ->'a)->'a->'a) (incr:'a->'a) (neutral:'a) = b incr (a incr neutral)

// we can construct more integers using sums
let two<'a>   = sum<'a> one one
let three<'a> = sum<'a> one two
let five<'a>  = sum<'a> two three
let seven<'a> = sum<'a> two five

// and recover the integer value
two   ((+)1) 0
three ((+)1) 0
five  ((+)1) 0
seven ((+)1) 0

// we can compute products
let prod<'a> (a:('a ->'a)->'a->'a) (b:('a ->'a)->'a->'a) (incr:'a->'a) (neutral:'a) = b (a incr) neutral

// we can construct more integers using products
let four<'a>  = prod<'a> two two
let six<'a>   = prod<'a> two three
let eight<'a> = prod<'a> two four
let nine<'a>  = prod<'a> three three

// and recover the integer value
four ((+)1) 0
six ((+)1) 0
eight ((+)1) 0
nine ((+)1) 0

// from now on, use a helper to recover the integer value
let eval integer: int = integer ((+)1) 0

// combine some things
(eval (sum one (prod two two))) = (eval (five))


// BOOLEANS and INTEGERS combined


// some predicate to test for zero
// (remember the concept of integers here, its a loop count for any loop)
let isZero integer = integer (ignore >> fun () -> False) True

// now we can evaluate such things:
isZero zero "yes" "no"   // writes "yes"
isZero five "yes" "no"   // writes "no"

// we can generate lists of things:
let fiveAs = five (fun ls -> "a" :: ls) []                 // gives ["a"; "a"; "a"; "a"; "a"]
let fiveAsAndOneB = five (fun ls -> "a" :: ls) [] @ ["b"]  // gives ["a"; "a"; "a"; "a"; "a"; "b"]

// we can generate a list of our integers
let countTo integer =
    let step (counter, ls) =
        let next = sum counter one
        (next, next :: ls)
    let initial = (zero<int>, [zero<int>])
    let (_, result) = integer step initial
    result

// and look at them
countTo five |> List.map eval

// we can count forward and thus find the predecessor
let decr integer =
    let step (counter, last) =
        let next = sum counter one
        (next, counter)
    let initial = (zero, zero)
    let (_, result) = integer step initial
    result

// and look at it
decr five |> eval

// (looking at 'countTo five', we are quite close to the 'fact' example
// shown around minute six in https://www.youtube.com/watch?v=XrNdvWqxBvA)
