

// what does a boolean do?
// a boolean makes a choice
let True x y = x
let False x y = y

// we can define if-then-else as follows
let IfTe bool t e = bool t e

// what does an integer do?
// an integer counts loop iterations
let two  iter x = iter (iter x)
let one  iter x = iter x
let zero iter x = x

// with explicit typing, to ensure uniform type signature
// bonus question: what is the consequence of more genericity?
let two<'a>  (iter:'a->'a) x = iter (iter x)
let one<'a>  (iter:'a->'a) x = iter x
let zero<'a> (iter:'a->'a) x = x

// we can evaluate the integer
// (given an inc-by-one as iter function and 0 as input value)
zero ((+)1) 0
one  ((+)1) 0
two  ((+)1) 0

// we can compute sums
let sum a b iter x = b iter (a iter x)

// again, with explicit typing for uniform type signatures
let sum<'a> (a:('a ->'a)->'a->'a) (b:('a ->'a)->'a->'a) iter x = b iter (a iter x)

// we can compute integers from sums
let three<'a> = sum<'a> one two
let five<'a>  = sum<'a> two three
let seven<'a> = sum<'a> two five

// and recover the integer value
three ((+)1) 0
five  ((+)1) 0
seven ((+)1) 0

// we can compute products
let prod<'a> (a:('a ->'a)->'a->'a) (b:('a ->'a)->'a->'a) iter x = b (a iter) x

// we can compute integers from products
let four<'a>  = prod<'a> two two
let six<'a>   = prod<'a> two three
let eight<'a> = prod<'a> two four
let nine<'a>  = prod<'a> three three

// and recover the integer value
four ((+)1) 0
six ((+)1) 0
eight ((+)1) 0
nine ((+)1) 0
