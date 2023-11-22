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

// let multngons(ngon: ngon): string =
//   let rec makeNString (n: int) (totalStep: int): string =
//     match n with
//     | 1 ->
//         let (x,y) = (double (fst ngon.center), double (snd ngon.center))
//         evalngon (ngon.radius) ((x,y)) (ngon.numSides) (ngon.color[0])
//     | _ ->
//         let (x,y) = (double (fst ngon.center), double (snd ngon.center))
//         let (dx,dy) = (double (fst ngon.centerDelta), double (snd ngon.centerDelta))
//         let (a,b) = (x+dx*(double(n-1)), y+dy*(double(n-1)))
//         evalngon
//           (double (ngon.radius + totalStep))
//           // ((x + dx*(n-1),y+dy*(n-1)))
//           (a,b)
//           (ngon.numSides)
//           (ngon.color[n-1])
//         + "\n" + (makeNString(n-1) (totalStep - ngon.step[n-2]))

//   let totalStep = ngon.step |> List.fold (fun acc x -> acc + x) 0
//   makeNString (ngon.num) (totalStep)

let rec extendList (size: int) (xs) =
  match xs with
  | y::ys when ys.Length = 0 && size > 1 -> y :: (extendList (size-1) (y::ys))
  | y::ys -> y :: (extendList (size-1)(ys))
  | [] -> []


let multngons(ngon: ngon): string =
  let rec makeNString (n: int) (totalStep: int) ((cx,cy): (double*double)): string =
    match n with
    | 1 ->
        let (x,y) = (double (fst ngon.center), double (snd ngon.center))
        evalngon (ngon.radius) ((x,y)) (ngon.numSides) (ngon.color[0])
    | _ ->
        // let (x,y) = (double (fst ngon.center), double (snd ngon.center))
        let (dx,dy) = (double (fst ngon.centerDelta[n-2]), double (snd ngon.centerDelta[n-2]))
        // let (a,b) = (x+dx*(double(n-1)), y+dy*(double(n-1)))
        // printf "%A" (cx,cy,dx,dy)
        evalngon
          (double (ngon.radius + totalStep))
          // ((x + dx*(n-1),y+dy*(n-1)))
          (cx, cy)
          (ngon.numSides)
          (ngon.color[n-1])
        + "\n" + (makeNString(n-1) (totalStep - ngon.step[n-2]) ((cx-dx, cy-dy)))

  let totalStep = ngon.step |> List.fold (fun acc x -> acc + x) 0
  let finalx = (fst ngon.center) + (ngon.centerDelta |> List.fold (fun acc x -> acc + (fst x)) 0)
  let finaly = (snd ngon.center) + (ngon.centerDelta |> List.fold (fun acc y -> acc + (snd y)) 0)
  makeNString (ngon.num) (totalStep) ((finalx,finaly))

let clean (ngon) =
  let realColors = ngon.color |> extendList ngon.num
  let realSteps = ngon.step |> extendList (ngon.num-1)
  let realDeltas = ngon.centerDelta |> extendList (ngon.num-1)
  {ngon with step=realSteps; color=realColors; centerDelta=realDeltas}

let allngons (ngonL: ngon list): string =
  ngonL |> 
    List.fold (fun acc ngon -> acc + (multngons (clean ngon)) + "\n") ""



let eval (data: (int * int) * ngon list): string =
  let s = $"viewBox=\"0 0 {fst (fst data)} {snd (fst data)}\" xmlns=\"http://www.w3.org/2000/svg\""
  $"<svg {s}>\n{allngons (snd data)}\n</svg>"