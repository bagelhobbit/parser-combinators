module ParserTests

open System
open FsCheck
open Expecto

open Parser


type DigitGenerator() =
    static member Digit() : Arbitrary<string> =
        gen {
            let! i = Gen.choose (0, 9)
            return sprintf "%d" i
        } |> Arb.fromGen

let createStringTestInput input = 
    let intialInput = TextInput.fromStr input
    let separators = [| "\r\n"; "\n" |]
    let lines = input.Split(separators, StringSplitOptions.None)
    let colPos =
        if lines.Length > 1
        then
            lines.[lines.Length - 1].Length
        else
            input.Length 
    let newPos : TextInput.Position = {line=lines.Length - 1; column=colPos}
    {intialInput with position=newPos}

let createCharTestInput char = createStringTestInput (sprintf "%c" char)

[<Tests>]
let combinatorTests =
    testList "Combinator Tests" [
        testProperty "orElse matches either character" <| fun (x:char) (y:char) ->
            let orParser = pchar x <|> pchar y
            (Ok (x, createCharTestInput x) = run orParser (sprintf "%c" x)) |@ "Didn't match x" .&.
            (Ok (y, createCharTestInput y) = run orParser (sprintf "%c" y)) |@ "Didn't match y"

        testProperty "andThen matches both characters" <| fun (x:char) (y:char) ->
            let input = (sprintf "%c%c" x y)
            Ok ((x,y), createStringTestInput input) = run (pchar x .>>. pchar y) input

        testProperty "andThen and orElse combine properly" <| fun (a:char) (b:char) (c:char) (d:char) ->
            let parser = (pchar a .>>. pchar b) <|> (pchar c .>>. pchar d)
            let ab = sprintf "%c%c" a b
            let cd = sprintf "%c%c" c d
            (Ok ((a,b), createStringTestInput ab) = run parser ab) |@ "Didn't match 'ab'" .|.
            (Ok ((c,d), createStringTestInput cd) = run parser cd) |@ "Didn't match 'cd'"

        testProperty "Map applies function to parser value" <| fun (c:char) ->
            let parserToInt = mapP int (pchar c)
            Ok ((int c), createCharTestInput c) = run parserToInt (sprintf "%c" c)

        testProperty "Return wraps string value" <| fun (s:string) ->
            Ok (s, createStringTestInput "") = run (returnP s) ""

        testProperty "Return wraps char value" <| fun (c:char) ->
            Ok (c, createStringTestInput "") = run (returnP c) ""

        testProperty "Apply applies wrapped function" <| fun (c:char) ->
            let intParser = returnP int
            let parser = pchar c
            Ok ((int c), createCharTestInput c) = run (intParser <*> parser) (sprintf "%c" c)

        testProperty "Sequence creates a parser list from a list of parsers" <| fun (a:char) (b:char) (c:char) ->
            let parsers = [pchar a; pchar b; pchar c]
            let combined = sequence parsers
            let input = (sprintf "%c%c%c" a b c)
            Ok ([a; b; c], createStringTestInput input) = run combined input

        testProperty "Many matches repeated characters" <| fun (c:char) ->
            let input = String.replicate 5 (sprintf "%c" c)
            // Don't test with new line since it's too complicated to fix this test.
            // nextChar appends a '\n' when the end of the input is reached,
            // so we end up matching 6 '\n's instead of the 5 we passed
            c <> '\n' ==> ( Ok ([c; c; c; c; c], createStringTestInput input) = run (many (pchar c)) input )

        testCase "Many matches with no characters" <| fun _ ->
            let subject = run (many (pchar 'a')) "bbbbbb"
            Expect.isOk subject "Many won't fail with no matches"

        testProperty "Many1 matches repeated characters" <| fun (c:char) ->
            let input = String.replicate 5 (sprintf "%c" c)
            // Don't test with new line since it's too complicated to fix this test.
            // nextChar appends a '\n' when the end of the input is reached,
            // so we end up matching 6 '\n's instead of the 5 we passed
            c <> '\n' ==> ( Ok ([c; c; c; c; c], createStringTestInput input) = run (many1 (pchar c)) input )

        testCase "Many1 fails with no characters" <| fun _ ->
            let subject = run (many1 (pchar 'a')) "bbbbbb"
            Expect.isError subject "Many1 should fail with no matches"

        testProperty "sepBy1 parses a list correctly" <| fun (a:char) (b:char) (c:char) ->
            let input = sprintf "%c,%c,%c" a b c
            let commaP = pchar ','
            let charP = pchar a <|> pchar b <|> pchar c
            Ok ([a; b; c], createStringTestInput input) = run (sepBy1 charP commaP) input

        testProperty "opt returns Some for matches" <| fun (c:char) ->
            Ok (Some c, createCharTestInput c) = run (opt (pchar c)) (sprintf "%c" c)

        testProperty "opt returns None for no match" <| fun (a:char) (b:char) ->
            let finalInput = 
                let input = createCharTestInput b
                {input with position={line=0; column=0}}
            a <> b ==> ( Ok (None, finalInput) = run (opt (pchar a)) (sprintf "%c" b) )
    ]

let digitGenConfig =
    { FsCheckConfig.defaultConfig with 
        arbitrary = [typeof<DigitGenerator>]
    }

[<Tests>]
let parserTests =
    testList "Parser Tests" [
        testProperty "Char parser matches itself" <| fun (c:char) -> 
            Ok (c, createCharTestInput c) = run (pchar c) (sprintf "%c" c)

        testCase "Char parser returns error for no match" <| fun _ -> 
            let subject = run (pchar 'a') "was"
            Expect.isError subject "No match returns an error"

        testCase "Char parser returns error for empty string" <| fun _ -> 
            let subject = run (pchar 'a') ""
            Expect.isError subject "Empty string is an error"

        testProperty "String parser matches itself" <| fun (s:string) -> 
            not (String.IsNullOrEmpty(s))==> lazy ( Ok (s, createStringTestInput s) = run (pstring s) (sprintf "%s" s) )

        testCase "String parser returns error for no match" <| fun _ -> 
            let subject = run (pstring "test") "some test string"
            Expect.isError subject "No match returns an error"

        testCase "String parser returns error for empty string" <| fun _ -> 
            let subject = run (pstring "a") ""
            Expect.isError subject "Empty string is an error"

        testProperty "Int parser matches an int" <| fun (i:int) ->
            let input = (sprintf "%d" i)
            Ok (i, createStringTestInput input) = run pint input

        testPropertyWithConfig digitGenConfig "AnyOf for digits matches any digit" <| fun (s:string) -> 
            let digitParser = anyOf ['0'..'9']
            let result = run digitParser s
            Expect.isOk result
    ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args