open System

open Parser

// commonmark spec
// https://spec.commonmark.org/0.29/

type Header = 
    | H1 of string 
    | H2 of string 
    | H3 of string 
    | H4 of string 
    | H5 of string 
    | H6 of string


type Spec =
    | Text of string
    | Header of Header


module Header =
    let render = function
    | H1 h -> sprintf "<h1>%s</h1>" h
    | H2 h -> sprintf "<h2>%s</h2>" h
    | H3 h -> sprintf "<h3>%s</h3>" h
    | H4 h -> sprintf "<h4>%s</h4>" h
    | H5 h -> sprintf "<h5>%s</h5>" h
    | H6 h -> sprintf "<h6>%s</h6>" h

module Spec =
    let render = function
    | Text t -> t
    | Header h -> Header.render h

[<EntryPoint>]
let main args =
    let trueP = pstring "true"
    let falseP = pstring "false"
    let boolP = trueP <|> falseP

    // let result1 = run trueP "truX"
    // printResult result1

    // let result2 = run boolP "truX"
    // printResult result2

    let input = 
        """ # This is a heading
  ## Second heading
   ### Sub header
## This is another heading
##
    # This is not a valid heading
#### H4
##### H5
###### H6
#5 bolt
######## too many hashes
#hashtag
\## still not a header
# Test for *inline* \*parsing\*
#                  foo                                 
### Header endings ##     
### Header endings 2 #########  
### Invalid Header ending ##### b
# foo#
### foo \###

"""

    let textP = 
        let text = satisfy (fun c -> c <> '\n') "text" 
        // Only parse a single line
        many text .>> pchar '\n' |>> charListToStr |>> Text <?> "text"

    let atxHeadingP = 
        let hash = pchar '#'
        let heading = satisfy (fun ch -> ch <> '\n') "text"

        let space = pchar ' '

        // Parse and ignore the trailing newline so the parser moves past it.
        let headingContent = many1 space >>. many heading .>> pchar '\n' |>> charListToStr

        let contentOrNewline = headingContent <|> (pstring "\n" >>. returnP "")

        let upTo3Spaces = opt space .>>. opt space .>>. opt space

        let h1 = upTo3Spaces >>. hash >>. contentOrNewline |>> ((fun s -> s.Trim()) >> H1) <?> "h1"
        let h2 = upTo3Spaces >>. hash >>. hash >>. contentOrNewline |>> H2 <?> "h2"
        let h3 = upTo3Spaces >>. hash >>. hash >>. hash >>. contentOrNewline |>> H3 <?> "h3"
        let h4 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline |>> H4 <?> "h4"
        let h5 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline |>> H5 <?> "h5"
        let h6 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline |>> H6 <?> "h6"
        h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6 |>> Header <|> textP

    let headingResult = run (many1 atxHeadingP <?> "heading text") input
    printResult headingResult

    printfn ""

    match headingResult with
    | Ok (specs, _) -> specs |> List.map ( fun s -> printfn "%s" (Spec.render s) ) |> ignore
    | Error _-> printfn "Error parsing document"

    0