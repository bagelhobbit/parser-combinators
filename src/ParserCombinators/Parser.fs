namespace Parser

open System

type Parser<'T> = Parser of (string -> Result<('T * string), string>)


module Parser =
    let run parser input =
        let (Parser innerFn) = parser
        innerFn input

    let pchar charToMatch =
        let innerFn input =
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
        Parser innerFn

    let orElse parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input
            match result1 with
            | Ok _ -> result1
            | Error e1 -> 
                let result2 = run parser2 input
                match result2 with
                | Ok _ -> result2
                | Error e2 -> Error (sprintf "%s or %s" e1 e2)
        Parser innerFn

    let ( <|> ) = orElse

    let andThen parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input
            match result1 with
            | Ok (val1, remaining1) -> 
                let result2 = run parser2 remaining1
                match result2 with
                | Ok (val2, remaining2) -> Ok ((val1, val2), remaining2)
                | Error e -> Error (sprintf "%A and %s" val1 e)
            | Error e -> Error e
        Parser innerFn

    let ( .>>. ) = andThen

    let map f parser = 
        let innerFn input =
            let result = run parser input
            match result with
            | Ok (v, remaining) -> Ok ((f v), remaining)
            | Error e -> Error e
        Parser innerFn

    let ( <!> ) = map

    let rec many parser =
        let innerFn input =
            let result = run parser input
            match result with
            | Ok (v, remaining1) -> 
                let recResult = run (many parser) remaining1
                match recResult with
                | Ok (v2, remaining2) -> Ok (v::v2, remaining2)
                | Error _ -> Ok ([v], remaining1)
            | Error _ -> Ok ([], input)
        Parser innerFn

    let choice parserList = 
        List.reduce orElse parserList

    let anyOf charList =
        charList
        |> List.map pchar
        |> choice

    let returnP x =
        let innerFn input = Ok(x, input)
        Parser innerFn

    let apply fP xP =
        (fP .>>. xP)
        |> map (fun (f,x) -> f x)

    let (<*>) = apply