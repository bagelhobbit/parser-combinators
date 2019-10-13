module ParserTests

open System
open FsCheck
open Expecto

open Parser.Parser


type DigitGenerator() =
    static member Digit() : Arbitrary<string> =
        gen {
            let! i = Gen.choose (0, 9)
            return sprintf "%d" i
        } |> Arb.fromGen


[<Tests>]
let pcharTests =
    testList "pchar Tests" [
        testProperty "Parser for a char matches itself" <|
            fun (c:char) -> Ok (c,"") = run (pchar c) (sprintf "%c" c)

        testProperty "Parser returns remaining string" <|
            fun (c:char) (s:string) -> not (String.IsNullOrEmpty(s)) ==> ( Ok (c,s) = run (pchar c) (sprintf "%c%s" c s) )

        testCase "Parser return error for no match" <| fun _ -> 
            let subject = run (pchar 'a') "was"
            Expect.isError subject "No match returns an error"

        testCase "Parser returns error for empty string" <| fun _ -> 
            let subject = run (pchar 'a') ""
            Expect.isError subject "Empty string is an error"
    ]

[<Tests>]
let pstringTests =
    testList "pstring Tests" [
        testProperty "Parser for a string matches itself" <| fun (s:string) -> 
            not (String.IsNullOrEmpty(s))==> lazy ( Ok (s,"") = run (pstring s) (sprintf "%s" s) )

        testProperty "Parser returns remaining string" <| fun (s1:string) (s2:string) ->
            let condition = not ( String.IsNullOrEmpty(s1) || String.IsNullOrEmpty(s2) )
            condition ==> lazy ( Ok (s1,s2) = run (pstring s1) (sprintf "%s%s" s1 s2) )

        testCase "Parser return error for no match" <| fun _ -> 
            let subject = run (pstring "test") "some test string"
            Expect.isError subject "No match returns an error"

        testCase "Parser returns error for empty string" <| fun _ -> 
            let subject = run (pstring "a") ""
            Expect.isError subject "Empty string is an error"
    ]

[<Tests>]
let pintTests =
    testList "pint Tests" [
        testProperty "Parser for an int matches an int" <| fun (i:int) -> 
            Ok (i,"") = run pint (sprintf "%d" i)
    ]

[<Tests>]
let andOrTests =
    testList "andThen, orElse combinators" [
        testProperty "orElse matches either character" <| fun (x:char) (y:char) ->
            let orParser = pchar x <|> pchar y
            (Ok (x,"") = run orParser (sprintf "%c" x)) |@ "Didn't match x" .&.
            (Ok (y,"") = run orParser (sprintf "%c" y)) |@ "Didn't match y"

        testProperty "andThen matches both characters" <| fun (x:char) (y:char) ->
            Ok ((x,y),"") = run (pchar x .>>. pchar y) (sprintf "%c%c" x y)

        testProperty "andThen and orElse combine properly" <| fun (a:char) (b:char) (c:char) (d:char) ->
            let parser = (pchar a .>>. pchar b) <|> (pchar c .>>. pchar d)
            let ab = sprintf "%c%c" a b
            let cd = sprintf "%c%c" c d
            (Ok ((a,b), "") = run parser ab) |@ "Didn't match 'ab'" .|.
            (Ok ((c,d), "") = run parser cd) |@ "Didn't match 'cd'"
    ]
[<Tests>]
let combinatorTests =
    testList "Combinator Tests" [
        testProperty "Map applies function to parser value" <| fun (c:char) ->
            let parserToInt = mapP int (pchar c)
            Ok ((int c), "") = run parserToInt (sprintf "%c" c)

        testProperty "Return wraps string value" <| fun (s:string) ->
            Ok (s, "") = run (returnP s) ""

        testProperty "Return wraps char value" <| fun (c:char) ->
            Ok (c, "") = run (returnP c) ""

        testProperty "Apply applies wrapped function" <| fun (c:char) ->
            let intParser = returnP int
            let parser = pchar c
            Ok ((int c), "") = run (intParser <*> parser) (sprintf "%c" c)

        testProperty "Sequence creates a parser list from a list of parsers" <| fun (a:char) (b:char) (c:char) ->
            let parsers = [pchar a; pchar b; pchar c]
            let combined = sequence parsers
            Ok ([a; b; c], "") = run combined (sprintf "%c%c%c" a b c)
    ]

let digitGenConfig =
    { FsCheckConfig.defaultConfig with 
        arbitrary = [typeof<DigitGenerator>]
    }

[<Tests>]
let choiceTests =
    testList "Choice Tests" [
        testPropertyWithConfig digitGenConfig "AnyOf for digits matches any digit" <| fun (s:string) -> 
            let digitParser = anyOf ['0'..'9']
            let result = run digitParser s
            Expect.isOk result

        testProperty "Many matches repeated characters" <| fun (c:char) ->
            let input = String.replicate 5 (sprintf "%c" c)
            Ok ([c; c; c; c; c], "") = run (many (pchar c)) input

        testCase "Many matches with no characters" <| fun _ ->
            let subject = run (many (pchar 'a')) "bbbbbb"
            Expect.isOk subject "Many won't fail with no matches"

        testProperty "Many1 matches repeated characters" <| fun (c:char) ->
            let input = String.replicate 5 (sprintf "%c" c)
            Ok ([c; c; c; c; c], "") = run (many1 (pchar c)) input

        testCase "Many1 fails with no characters" <| fun _ ->
            let subject = run (many1 (pchar 'a')) "bbbbbb"
            Expect.isError subject "Many1 should fail with no matches"

        testProperty "sepBy1 parses a list correctly" <| fun (a:char) (b:char) (c:char) ->
            let input = sprintf "%c,%c,%c" a b c
            let commaP = pchar ','
            let charP = pchar a <|> pchar b <|> pchar c
            Ok ([a; b; c], "") = run (sepBy1 charP commaP) input 
    ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args