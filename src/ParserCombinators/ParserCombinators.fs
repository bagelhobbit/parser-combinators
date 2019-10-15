module ParserCombinators

open Parser

// TODO: backtracking
// In an or expression we don't want to backtrack if we were successful but failed later.
// See https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/ for more details.

[<EntryPoint>]
let main argv =
    let startsWith (str:string) (prefix:string) =
        str.StartsWith(prefix)

    let startsWithP = lift2 startsWith

    let x = startsWithP (returnP "test string") (returnP "test")
    let result = run x ""

    printfn "%A" result
    0 // return an integer exit code