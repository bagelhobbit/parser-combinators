module ParserCombinators

open Parser.Parser

[<EntryPoint>]
let main argv =
    let parser = pchar 'a'
    let okResult = run parser "abc"
    let errorResult = run parser "b"

    let orParser = orElse (pchar 'a') (pchar 'b')
    let infixOr = pchar 'a' <|> pchar 'b'

    let r1 = run orParser "abc"
    let r2 = run orParser "bcd"
    let r3 = run orParser "efg"

    let andParser = andThen (pchar 'a') (pchar 'b')
    let infixAnd = pchar 'a' .>>. pchar 'b'

    let r4 = run andParser "abc"
    let r5 = run andParser "acdc"
    let r6 = run andParser "bcd"

    let andOrParser = orElse (andThen (pchar 'a') (pchar 'b')) (andThen (pchar 'c') (pchar 'd'))
    let infixAndOr = (pchar 'a' .>>. pchar 'b') <|> (pchar 'c' .>>. pchar 'd')

    let r7 = run andOrParser "abef"
    let r8 = run andOrParser "cdef"
    let r9 = run andOrParser "ace"
    let r10 = run andOrParser "cab"

    let litToInt c = map int (pchar c)

    let intParser = orElse ( andThen (litToInt 'a') (litToInt 'b') ) ( andThen (litToInt 'c') (litToInt 'd') )

    let r11 = run intParser "abef"
    let r12 = run intParser "cdef"
    let r13 = run intParser "ace"
    let r14 = run intParser "cab"

    let manyParser = many (pchar 'a')

    let r15 = run manyParser "aaaaaa"
    let r16 = run manyParser "abbbbb"
    let r17 = run manyParser "bbbbbb"

    let digitParser = anyOf ['1'..'9']

    let r18 = run digitParser "1"
    let r19 = run digitParser "5"
    let r20 = run digitParser "9"

    let returnParser = returnP "abcd"

    let r21 = run returnParser ""

    // move into unit tests
    printfn "%A, %A" okResult errorResult
    printfn "%A, %A, %A" r1 r2 r3
    printfn "%A, %A, %A" r4 r5 r6
    printfn "%A, %A, %A, %A" r7 r8 r9 r10
    printfn "%A, %A, %A, %A" r11 r12 r13 r14
    printfn "%A, %A, %A" r15 r16 r17
    printfn "%A, %A, %A" r18 r19 r20
    printfn "%A" r21
    0 // return an integer exit code