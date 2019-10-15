module Parser

module TextInput =
    open System

    type Position = {
        line : int
        column : int
    }

    let initialPos = {line=0;column=0}

    let incrCol pos = {pos with column=pos.column + 1}

    let incrLine pos = {line=pos.line + 1; column=0}

    type InputState = {
        lines : string []
        position : Position
    }

    let currentLine inputState =
        let linePos = inputState.position.line
        if linePos < inputState.lines.Length
        then
            inputState.lines.[linePos]
        else
            "end of file"

    let fromStr str =
        if String.IsNullOrEmpty(str)
        then
            {lines=[||]; position=initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            {lines=lines; position=initialPos}

    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column
        // three cases
        // 1) if line >= maxLine -> 
        //       return EOF
        // 2) if col less than line length -> 
        //       return char at colPos, increment colPos
        // 3) if col at line length -> 
        //       return NewLine, increment linePos

        if linePos >= input.lines.Length
        then
            input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length
            then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position
                let newState = {input with position=newPos}
                newState, Some char
            else
                // End of line, return LF and move to next line
                let char = '\n'
                let newPos = incrLine input.position
                let newState = {input with position=newPos}
                newState, Some char

open System

type Input = TextInput.InputState
type ParserLabel = string
type ParserError = string

type ParserPosition = { 
    currentLine : string
    line : int
    column : int 
}

type Parser<'T> = { 
    parseFn : Input -> Result<('T * Input), ParserLabel * ParserError * ParserPosition>
    label : ParserLabel 
}


let runOnInput parser input =
    parser.parseFn input

let run parser inputStr =
    runOnInput parser (TextInput.fromStr inputStr)

let parserPositionFromInputState (inputState:Input) = {
    currentLine = TextInput.currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
}

let printResult result =
    match result with
    | Ok (value, _) -> printfn "%A" value
    | Error (label, error, parserPos) -> 
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let getLabel parser = parser.label

let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Ok (v, remaining) -> Ok (v, remaining)
        | Error (_, err, pos) -> Error (newLabel, err, pos)
    { parseFn=newInnerFn; label=newLabel}

let ( <?> ) = setLabel

[<AutoOpen>]
module Combinators =
    let satisfy predicate label =
        let innerFn input =
            let remainingInput, charOpt = TextInput.nextChar input
            match charOpt with
            | Some first ->
                if predicate first 
                then
                    Ok (first, remainingInput)
                else
                    let error = sprintf "Unexpected '%c'" first
                    let pos = parserPositionFromInputState input
                    Error (label, error, pos)
            | None ->
                let error = "No more input"
                let pos = parserPositionFromInputState input
                Error (label, error, pos)
        {parseFn=innerFn; label=label}

    let bindP f p =
        let label = "unknown"
        let innerFn input =
            let result1 = runOnInput p input
            match result1 with
            | Ok (v, remaining) -> 
                let p2 = f v
                runOnInput p2 remaining
            | Error e -> Error e
        {parseFn=innerFn; label=label}

    let ( >>= ) p f = bindP f p

    let returnP x =
        let label = "unknown"
        let innerFn input = Ok(x, input)
        {parseFn=innerFn; label=label}

    let mapP f = 
        bindP (f >> returnP)

    let ( <!> ) = mapP
    let (|>>) x f = mapP f x

    let applyP fP xP =
        fP >>= (fun f ->
        xP >>= (fun x ->
            returnP (f x) ))

    let (<*>) = applyP

    let lift2 f xP yP =
        returnP f <*> xP <*> yP

    let andThen p1 p2 =
        let label = sprintf "%s andThen %s" p1.label p2.label
        p1 >>= (fun p1result ->
        p2 >>= (fun p2result ->
            returnP (p1result, p2result) ))
        <?> label
    let ( .>>. ) = andThen

    let orElse p1 p2 =
        let label = sprintf "%s orElse %s" p1.label p2.label
        let innerFn input =
            let result1 = runOnInput p1 input
            match result1 with
            | Ok _ -> result1
            | Error _ -> 
                let result2 = runOnInput p2 input
                match result2 with
                | Ok _ -> result2
                | Error (_, error, pos) -> Error (label, error, pos)
        {parseFn=innerFn; label=label}

    let ( <|> ) = orElse

    let choice parserList = 
        List.reduce orElse parserList

    let rec sequence parserList =
        let cons head tail = head::tail
        let consP = lift2 cons

        match parserList with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

    let rec private parseZeroOrMore parser input =
        let firstResult = runOnInput parser input
        match firstResult with
        | Ok (firstValue, inputAfterParse) ->
            let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterParse
            let values = firstValue::subsequentValues
            (values, remainingInput)
        | Error _ -> ([], input)

    let rec many parser =
        let label = sprintf "many %s" parser.label
        let innerFn input =
            Ok (parseZeroOrMore parser input)
        { parseFn=innerFn; label=label }

    let rec many1 parser =
        let label = sprintf "many1 %s" parser.label

        parser >>= (fun head ->
        many parser >>= (fun tail ->
            returnP (head::tail) ))
        <?> label

    let opt parser =
        let label = sprintf "opt %s" parser.label
        let some = parser |>> Some
        let none = returnP None
        some <|> none
    let ( .>> ) p1 p2 =
        p1 .>>. p2
        |> mapP (fun (x,_) -> x)

    let ( >>. ) p1 p2 =
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

[<AutoOpen>]
module StdParsers =
    let pchar charToMatch =
        let predicate ch = (ch = charToMatch)
        let label = sprintf "%c" charToMatch

        satisfy predicate label

    let anyOf charList =
        let label = sprintf "any of %A" charList
        charList
        |> List.map pchar
        |> choice
        <?> label

    let charListToStr charList =
        String(List.toArray charList)

    let manyChars cp =
        many cp
        |>> charListToStr

    let manyChars1 cp =
        many1 cp
        |>> charListToStr

    let pstring str =
        let label = str

        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP charListToStr
        <?> label

    let whitespaceChar =
        let predicate = Char.IsWhiteSpace
        let label = "whitespace"
        satisfy predicate label

    let spaces = many whitespaceChar

    let spaces1 = many1 whitespaceChar


    let digitChar =
        let predicate = Char.IsDigit
        let label = "digit"
        satisfy predicate label

    let pint =
        let label = "integer"

        let resultToInt (sign, digits) =
            let i = digits |> int
            match sign with
            | Some _ -> -i
            | None -> i

        let digits = manyChars1 digitChar

        opt (pchar '-') .>>. digits 
        |> mapP resultToInt
        <?> label

    let pfloat = 
        let label = "float"

        let resultToFloat (((sign, digits1), _), digits2) =
            let float = sprintf "%s.%s" digits1 digits2 |> float
            match sign with
            | Some _ -> -float
            | None -> float 

        let digits = manyChars1 digitChar

        // ignore parsing exponents for now
        opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits
        |> mapP resultToFloat
        <?> label