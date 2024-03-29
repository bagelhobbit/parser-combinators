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

    let stringListToString = function
        | [] -> ""
        | [s] -> s
        | xs -> xs |> List.reduce (+)

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

        let upTo3Spaces = opt space .>>. opt space .>>. opt space

        let opt6PairToList (((((a, b), c), d), e), f) =
            (Some a)::b::c::d::e::[f]
            |> List.collect Option.toList

        let oneToSixHashes = hash .>>. opt hash .>>. opt hash .>>. opt hash .>>. opt hash .>>. opt hash |>> opt6PairToList

        let notNewline = satisfy (fun ch -> ch <> '\n' && ch <> '#' && ch <> ' ' && ch <> '\\') "text" |>> sprintf "%c" <|> ( pchar '\\' >>. many1 hash |>> charListToStr ) <|> pstring "\\"

        let spacesThenContent = many1 space .>>. many notNewline |>> (fun (a, b) -> (charListToStr a) :: b)
        let spacesThenHash = many space .>>. many hash .>> many space |>> (fun (a, b) -> a @ b)

        let contentOrEmpty =  ( many spacesThenContent .>> spacesThenHash ) <|> (many space >>. returnP [[""]]) |>> List.concat

        let getCanonicalHeader (hashes, strings) =
            let header = 
                match hashes with
                | 1 -> H1
                | 2 -> H2
                | 3 -> H3
                | 4 -> H4
                | 5 -> H5
                | _ -> H6
            
            strings |> stringListToString |> canonicalize |> header

        let header = upTo3Spaces >>. ( oneToSixHashes |>> (fun chars -> chars.Length) ) .>>. contentOrEmpty .>> pchar '\n' |>> getCanonicalHeader <?> "atx heading"

        header |>> Header <|> textP


    let headingResult = run (many1 atxHeadingP <?> "heading text") input
    printResult headingResult

    printfn ""

    match headingResult with
    | Ok (specs, _) -> specs |> List.map ( fun s -> printfn "%s" (Spec.render s) ) |> ignore
    | Error _-> printfn "Error parsing document"

    0