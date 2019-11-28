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
### Header endings 3 #
### Invalid Header ending ##### b
# foo#
### foo \###

"""

    let charToStr c = sprintf "%c" c

    let stringListToString (strings:string list) = 
        strings
        |> List.reduce (+)

    let text = satisfy (fun c -> c <> '\n') "text" 

    let removeEscapedCharacters s =
        let parser = many ( opt (pchar '\\') >>. text ) .>> pchar '\n' |>> charListToStr

        match run parser s with
        | Ok (result,_) -> result
        | Error _ -> s

    let canonicalize =
        let trim (s:string) = s.Trim()
        trim >> removeEscapedCharacters

    let textP = 
        // Only parse a single line
        many text .>> pchar '\n' |>> charListToStr |>> (canonicalize >> Text) <?> "text"

    let atxHeadingP =
        let hash = pchar '#'
        let space = pchar ' '

        let contentOrNewline =
            let heading = 
                let basicText = satisfy (fun ch -> ch <> '\n' && ch <> '#') "text" |>> charToStr
                let escapedHash = pstring "\\#" .>>. many (pstring "#") |>> (fun (s1,s2) -> s1 + (stringListToString s2))
                escapedHash <|> basicText
            let headingContent = many1 space >>. many heading |>> stringListToString
            headingContent <|> (pstring "\n" >>. returnP "")

        let upTo3Spaces = opt space .>>. opt space .>>. opt space

        // ATX headers can be optionally closed by any number of hashes
        let closingHashes = 
            let endingHashes = many space .>> many hash .>> many space 
            let trailingSpaces = many space
            (endingHashes <|> trailingSpaces) .>> pchar '\n'

        let h1 = upTo3Spaces >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H1) <?> "h1"
        let h2 = upTo3Spaces >>. hash >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H2) <?> "h2"
        let h3 = upTo3Spaces >>. hash >>. hash >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H3) <?> "h3"
        let h4 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H4) <?> "h4"
        let h5 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H5) <?> "h5"
        let h6 = upTo3Spaces >>. hash >>. hash >>. hash >>. hash >>. hash >>. hash >>. contentOrNewline .>> closingHashes |>> (canonicalize >> H6) <?> "h6"

        let test = run h3 "### Header endings ##     "
        printResult test

        h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6 |>> Header <|> textP

    let headingResult = run (many1 atxHeadingP <?> "heading text") input
    printResult headingResult

    printfn ""

    match headingResult with
    | Ok (specs, _) -> specs |> List.map ( fun s -> printfn "%s" (Spec.render s) ) |> ignore
    | Error _-> printfn "Error parsing document"

    0