module ParserCombinators

open Parser.Parser

[<EntryPoint>]
let main argv =
    let startsWith (str:string) (prefix:string) =
        str.StartsWith(prefix)

    let startsWithP = lift2 startsWith

    let x = startsWithP (returnP "test string") (returnP "test")
    let result = run x ""

    printfn "%A" result
    0 // return an integer exit code