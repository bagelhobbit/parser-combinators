module ParserTests

open System
open FsCheck

open Parser.Parser


module test =
    // Test for errors, not just happy path
    let ``Parser For A Char Matches Itself`` (c:char) = Ok (c,"") = run (pchar c) (sprintf "%c" c)

    let ``Parser Returns Remaining String`` (c:char) (s:string) = not (String.IsNullOrEmpty(s)) ==> ( Ok (c,s) = run (pchar c) (sprintf "%c%s" c s) )

    let ``orElse Matches Either Character`` (x:char) (y:char) = 
        let orParser = pchar x <|> pchar y
        (Ok (x,"") = run orParser (sprintf "%c" x)) |@ "Didn't match x" .&.
        (Ok (y,"") = run orParser (sprintf "%c" y)) |@ "Didn't match y"

    let ``andThen Matches Both Characters`` (x:char) (y:char) = Ok ((x,y),"") = run (pchar x .>>. pchar y) (sprintf "%c%c" x y)

    // let andOrParser = orElse (andThen (pchar 'a') (pchar 'b')) (andThen (pchar 'c') (pchar 'd'))
    // let infixAndOr = (pchar 'a' .>>. pchar 'b') <|> (pchar 'c' .>>. pchar 'd')

    // let r7 = run andOrParser "abef"
    // let r8 = run andOrParser "cdef"
    // let r9 = run andOrParser "ace"
    // let r10 = run andOrParser "cab"

    // let litToInt c = map int (pchar c)

    // let intParser = orElse ( andThen (litToInt 'a') (litToInt 'b') ) ( andThen (litToInt 'c') (litToInt 'd') )

    // let r11 = run intParser "abef"
    // let r12 = run intParser "cdef"
    // let r13 = run intParser "ace"
    // let r14 = run intParser "cab"

    // let manyParser = many (pchar 'a')

    // let r15 = run manyParser "aaaaaa"
    // let r16 = run manyParser "abbbbb"
    // let r17 = run manyParser "bbbbbb"

    // let digitParser = anyOf ['1'..'9']

    // let r18 = run digitParser "1"
    // let r19 = run digitParser "5"
    // let r20 = run digitParser "9"

    // let returnParser = returnP "abcd"

    // let r21 = run returnParser ""


[<EntryPoint>]
let main argv =
    Check.Quick test.``Parser For A Char Matches Itself``
    Check.Quick test.``Parser Returns Remaining String``
    Check.Quick test.``orElse Matches Either Character``
    Check.Quick test.``andThen Matches Both Characters``
    0 // return an integer exit code