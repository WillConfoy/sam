open Parser
open Evaluator
open System.IO
open Combinator

// [<EntryPoint>]
// let main(argv: string[]): int = 
//   let input = prepare "2 7-gon with radius 20and center(100,50),step[10],color[#5f28a8,#FFFFFF],centerDelta(1,1),normal(0.0,1.0) and granularity 0"
//   // let grammar = pleft pngon peof
//   printfn "%A" (grammar input)
//   0

// "4 10-gon with radius 30and center(100,50),step[1,2,1],color[#f9c406,#FF0000,#995668,#f9c406],centerDelta(-5,2),laplace(0.0,10.0) and granularity 5"

// canvas (400,400)

[<EntryPoint>]
let main (args: string[]): int =
  try
    let file = args[0]
    let text = File.ReadAllText file
    match args.Length with
        | 1 ->
            let asto = parse text
            match asto with
                | Some ast ->
                    printfn "%s" (eval ast)
                    0
                | None -> 0
        | _ ->
            printfn "Usage: good question"
            1
  with
  | e -> printfn $"Error thrown! \n\n{e.Message}"; 1
