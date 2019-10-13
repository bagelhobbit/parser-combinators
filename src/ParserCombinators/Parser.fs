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

    let mapP f parser = 
        let innerFn input =
            let result = run parser input
            match result with
            | Ok (v, remaining) -> Ok ((f v), remaining)
            | Error e -> Error e
        Parser innerFn

    let ( <!> ) = mapP
    let (|>>) x f = mapP f x 

    let choice parserList = 
        List.reduce orElse parserList

    let anyOf charList =
        charList
        |> List.map pchar
        |> choice

    let returnP x =
        let innerFn input = Ok(x, input)
        Parser innerFn

    let applyP fP xP =
        (fP .>>. xP)
        |> mapP (fun (f,x) -> f x)

    let (<*>) = applyP

    let lift2 f xP yP =
        returnP f <*> xP <*> yP

    let rec sequence parserList =
        let cons head tail = head::tail
        let consP = lift2 cons

        match parserList with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)
    
    let charListToStr charList =
        String(List.toArray charList)

    let pstring str =
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP charListToStr


    let rec private parseZeroOrMore parser input =
        let firstResult = run parser input
        match firstResult with
        | Ok (firstValue, inputAfterParse) ->
            let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterParse
            let values = firstValue::subsequentValues
            (values, remainingInput)
        | Error _ -> ([], input)

    let rec many parser =
        let innerFn input =
            Ok (parseZeroOrMore parser input)
        Parser innerFn

    let rec many1 parser =
        let innerFn input =
            let result = run parser input
            match result with
            | Ok (firstValue, inputAfterFirstParse) ->
                let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
                let values = firstValue::subsequentValues
                Ok (values, remainingInput)
            | Error e -> Error e
        Parser innerFn

    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let pint =
        let resultToInt (sign, charList) =
            let i = String(List.toArray charList) |> int
            match sign with
            | Some _ -> -i
            | None -> i

        let digit = anyOf ['0'..'9']

        let digits = many1 digit

        opt (pchar '-') .>>. digits 
        |> mapP resultToInt

    let (.>>) p1 p2 =
        p1 .>>. p2
        |> mapP (fun (x,_) -> x)

    let (>>.) p1 p2 =
        p1 .>>. p2
        |> mapP (fun (_,x) -> x)

    let between p1 p2 p3 =
        p1 >>. p2 .>> p3

    // Parses one or more occurences of p separated by sep
    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p, pList) -> p::pList

    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    let bindP f p =
        let innerFn input =
            let result1 = run p input
            match result1 with
            | Ok (v, remaining) -> 
                let p2 = f v
                run p2 remaining
            | Error e -> Error e
        Parser innerFn

    let ( >>= ) p f = bindP f p