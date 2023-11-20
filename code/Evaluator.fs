module Evaluator

open AST
open System

let createPoints(r: double)((x,y): double * double)(n: int): (double * double) list =
  let thetad = double (360 / n)
  let theta = (thetad * Math.PI) / (double 180)

  let rec makePList (n: int): double list =
    match n with
    | 0 -> [0]
    | _ -> theta * (double n) :: makePList(n-1)

  makePList(n) |> List.rev
               |> List.map (fun t -> (Math.Round((r * Math.Cos(t) + x),3),Math.Round((r * Math.Sin(t) + y),3)))
  


// let evalngon(ngon: ngon): string =
let evalngon(r: double)((x,y): double * double)(n: int)(c: string): string =
  // let (x,y) = (double (fst ngon.center), double (snd ngon.center))
  let points = createPoints (r) ((x,y)) (n)
  let pointString = 
      (points |> List.fold (fun acc (x,y) -> acc + $"{x},{y} ") "")
  $"<polygon points=\"{pointString}\" fill=\"{c}\"/>"

let multngons(ngon: ngon): string =
  let rec makeNString (n: int) (totalStep: int): string =
    match n with
    | 1 ->
        let (x,y) = (double (fst ngon.center), double (snd ngon.center))
        evalngon (ngon.radius) ((x,y)) (ngon.numSides) (ngon.color[0])
    | _ ->
        let (x,y) = (double (fst ngon.center), double (snd ngon.center))
        let (dx,dy) = (double (fst ngon.centerDelta), double (snd ngon.centerDelta))
        let (a,b) = (x+dx*(double(n-1)), y+dy*(double(n-1)))
        evalngon
          (double (ngon.radius + totalStep))
          // ((x + dx*(n-1),y+dy*(n-1)))
          (a,b)
          (ngon.numSides)
          (ngon.color[n-1])
        + "\n" + (makeNString(n-1) (totalStep - ngon.step[n-2]))

  let totalStep = ngon.step |> List.fold (fun acc x -> acc + x) 0
  makeNString (ngon.num) (totalStep)


let eval (ngon: ngon): string =
  let s = "viewBox=\"0 0 200 100\" xmlns=\"http://www.w3.org/2000/svg\""
  $"<svg {s}>\n{multngons ngon}\n</svg>"