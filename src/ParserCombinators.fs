module ParserCombinators

open System

let lit charToMatch =
    fun input ->
        if String.IsNullOrEmpty(input)
        then
            Error "No more input"
        else 
            let first = input.[0]
            if first = charToMatch
            then 
                let remaining = input.[1..]
                Ok (charToMatch, remaining)
            else 
                Error (sprintf " Expecting '%c'. Got '%c'" charToMatch first)

let Or parser1 parser2 =
    fun input ->
        let result1 = parser1 input
        match result1 with
        | Ok _ -> result1
        | Error e1 -> 
            let result2 = parser2 input
            match result2 with
            | Ok _ -> result2
            | Error e2 -> Error (sprintf "%s or %s" e1 e2)

let And parser1 parser2 =
    fun input ->
        let result1 = parser1 input
        match result1 with
        | Ok (val1, remaining) -> 
            let result2 = parser2 remaining
            match result2 with
            | Ok (val2, s) -> Ok ([val1; val2], s)
            | Error e -> Error (sprintf "%A and %s" val1 e)
        | Error e -> Error e

let apply f parser = 
    fun input ->
        let result = parser input
        match result with
        | Ok (v, remaining) -> Ok ((f v), remaining)
        | Error e -> Error e

let rec many parser =
    fun input ->
        let result = parser input
        match result with
        | Ok (v, remaining) -> 
            let recResult = many parser remaining
            match recResult with
            | Ok (v2, recRemain) -> Ok (v::v2, recRemain)
            | Error e -> Ok ([v], remaining)
        | Error _ -> Ok ([], input)

[<EntryPoint>]
let main argv =
    let parser = lit 'a'
    let okResult = parser "abc"
    let errorResult = parser "b"

    let orParser = Or (lit 'a') (lit 'b')

    let r1 = orParser "abc"
    let r2 = orParser "bcd"
    let r3 = orParser "efg"

    
    let andParser = And (lit 'a') (lit 'b')

    let r4 = andParser "abc"
    let r5 = andParser "acdc"
    let r6 = andParser "bcd"

    let andOrParser = Or (And (lit 'a') (lit 'b')) (And (lit 'c') (lit 'd'))

    let r7 = andOrParser "abef"
    let r8 = andOrParser "cdef"
    let r9 = andOrParser "ace"
    let r10 = andOrParser "cab"

    let litToInt c = apply int (lit c)

    let intParser = Or ( And (litToInt 'a') (litToInt 'b') ) ( And (litToInt 'c') (litToInt 'd') )

    let r11 = intParser "abef"
    let r12 = intParser "cdef"
    let r13 = intParser "ace"
    let r14 = intParser "cab"

    let manyParser = many (lit 'a')

    let r15 = manyParser "aaaaaa"
    let r16 = manyParser "abbbbb"
    let r17 = manyParser "bbbbbb"

    printfn "%A, %A" okResult errorResult
    printfn "%A, %A, %A" r1 r2 r3
    printfn "%A, %A, %A" r4 r5 r6
    printfn "%A, %A, %A, %A" r7 r8 r9 r10
    printfn "%A, %A, %A, %A" r11 r12 r13 r14
    printfn "%A, %A, %A" r15 r16 r17
    0 // return an integer exit code