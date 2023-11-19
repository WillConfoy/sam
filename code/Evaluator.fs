module Evaluator

open AST
open System

let createPoints(r: double)((x,y): double * double)(n: int): (double * double) list =
  let thetad = double (360 / n)
  let theta = (thetad * Math.PI) / (double 180)

  let rec makeList (n: int): double list =
    match n with
    | 0 -> [0]
    | _ -> theta * (double n) :: makeList(n-1)

  makeList(n) |> List.rev
              |> List.map (fun t -> (r * Math.Cos(t) + x, r * Math.Sin(t) + y))
  


let evalngon(ngon: ngon): string =
  let (x,y) = (double (fst ngon.center), double (snd ngon.center))
  let points = createPoints (ngon.radius) ((x,y)) (ngon.numSides)
  let pointString = 
      (points |> List.fold (fun acc (x,y) -> acc + $"{x},{y} ") "") + "\b"
  $"<polygon points=\"{pointString}\" fill=\"{ngon.color[0]}\"/>"


let eval (ngon: ngon): string =
  let s = "viewBox=\"0 0 200 100\" xmlns=\"http://www.w3.org/2000/svg\""
  $"<svg {s}> \n {evalngon ngon} \n </svg>"