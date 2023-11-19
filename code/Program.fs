open Parser
open Evaluator
open System.IO
open Combinator

// [<EntryPoint>]
// let main(argv: string[]): int = 
//   let input = prepare "1 7-gon with radius 20and center(1,1),step[],color[0x5f28a8],centerDelta(1,1),normal(0.0,1.0) and granularity 0"
//   // let grammar = pleft pngon peof
//   printfn "%A" (grammar input)
//   0

[<EntryPoint>]
let main (argv: string[]): int =
    match argv.Length with
        | 1 ->
            let asto = parse argv.[0]
            match asto with
                | Some ast ->
                    printfn "%A" (eval ast)
                    0
                | None -> 0
        | _ ->
            printfn "Usage: good question"
            1
