
//
// Part 1
//
// https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
//


// PLAIN

let log p = printfn "expression is %A" p

// workflow, side-effect: printfn during evaluation
let loggedWorkflow =
    let x = 42
    log x
    let y = 43
    log y
    let z = x + y
    log z
    //return
    z


// LOGGER WORKFLOW


type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member _.Bind(x, f) =
        log x
        f x

    member _.Return(x) =
        x

let logger = new LoggingBuilder()

let loggedWorkflow =
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }


// DIVISION BY ZERO


let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

let divideByWorkflow init x y z =
    let a = init |> divideBy x
    match a with
    | None -> None
    | Some a' ->
        let b = a' |> divideBy y
        match b with
        | None -> None
        | Some b' ->
            let c = b' |> divideBy z
            match c with
            | None -> None
            | Some c' ->
                // return
                Some c'  // yes, a bit redundant

let good = divideByWorkflow 12 3 2 1
let bad = divideByWorkflow 12 3 0 1


type MaybeBuilder() =
    member _.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a  // unpacking from Option!
    member _.Return(x) =
        Some x  // packing into Option

let maybe = MaybeBuilder()

let divdeByWorkflow init x y z =
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }

let good = divideByWorkflow 12 3 2 1
let bad = divideByWorkflow 12 3 0 1


// LOOKUP


let map1 = [ ("1", "One"); ("2", "Two") ] |> Map.ofList
let map2 = [ ("A", "Alice"); ("B", "Bob" )] |> Map.ofList
let map3 = [ ("CA", "California"); ("NY", "New York") ] |> Map.ofList

let multiLookup key =
    match map1.TryFind key with
    | Some result1 -> Some result1
    | None ->
        match map2.TryFind key with
        | Some result2 -> Some result2
        | None ->
        match map3.TryFind key with
        | Some result3 -> Some result3
        | None -> None

multiLookup "A"
multiLookup "DoesNotExist"

type OrElseBuilder() =
    member _.ReturnFrom(x) = x  // should wrap into Option, but if x is already Option, there is nothing to do
    member _.Combine (a, b) =
        match a with
        | Some _ -> a
        | None -> b
    member _.Delay(f) = f()

let orElse = OrElseBuilder()

let multiLookup key =
    orElse {
        return! map1.TryFind key
        return! map2.TryFind key
        return! map3.TryFind key
    }

multiLookup "B"
multiLookup "2"
multiLookup "666"
multiLookup "CA"


let overrideMap = [ ("A", "Anton"); ("1", "Eins") ] |> Map.ofList

let overrideLookup key =
    orElse {
        return! overrideMap.TryFind key
        return! multiLookup key
    }

overrideLookup "A"
overrideLookup "B"


//
// Part 2
//
// https://fsharpforfunandprofit.com/posts/computation-expressions-continuations/
//


let divide ifZero ifSuccess top bottom =
    if bottom = 0
    then ifZero()
    else ifSuccess (top / bottom)

divide (fun () -> printfn "bad") (fun a -> printfn "good %d" a) 12 2
divide (fun () -> None) (fun a -> Some a) 12 3
divide (fun () -> failwith "div by 0") (fun a -> a) 12 4


42 |> (fun x ->    // let x = 42
    43 |> (fun y ->    // let y = 43
        x + y |> (fun z ->    // let z = x+y
            z)))


let pipeInto (someExpression, lambda) =  // a tupple on purpose!
    someExpression |> lambda


pipeInto (42, fun x ->
pipeInto (43, fun y ->
pipeInto (x+y, fun z ->
z)))


//
//  Free Exercise
//

type StepBuilder() =
    member _.Bind(x, f) = fun () ->
        printfn "have %d" x
        f x
    member _.Return(x) = fun () ->
        printfn "have %d" x
        ()

let step = new StepBuilder()

let x = step {
    let! x = 5
    let! y = 6
    let! z = x + y
    return z
}

x ()  // performs one step
x () ()  // performs two steps
x () () () ()  // performs two steps


//
// Part 3
//
// https://fsharpforfunandprofit.com/posts/computation-expressions-bind/
//


//  |>   takes the value from the left and ALWAYS WILL pipe it UNMODIFIED to the function on the right    WITHOUT     side effects
//  >>=  takes the value from the left and    MAY      pipe it  MODIFIED  to the function on the right WITH ANY EXTRA side effects
//       (often the left hand value is somehow wrapped)


let (>>=) m f =
    printfn "value is %d" m
    f m

1 |>  (+) 2 |>  (*) 42  // using |>
1 >>= (+) 2 >>= (*) 42  // using >>=

let (>>=) m f =
    match m with
    | None   -> None
    | Some m -> f m

Some 12 >>= divideBy 3 >>= divideBy 2 >>= divideBy 1
Some 12 >>= divideBy 3 >>= divideBy 0 >>= divideBy 1
