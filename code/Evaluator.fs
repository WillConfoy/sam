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
let evalngon(r: double)((x,y): double * double)(n: int)(c: string)(rotate:float): string =
  // let (x,y) = (double (fst ngon.center), double (snd ngon.center))
  let points = createPoints (r) ((x,y)) (n)
  let pointString = 
      (points |> List.fold (fun acc (x,y) -> acc + $"{x},{y} ") "")
  $"<polygon points=\"{pointString}\" fill=\"{c}\" transform=\"rotate({rotate}, {x},{y})\"/>"

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
        evalngon (ngon.radius) ((x,y)) (ngon.numSides) (ngon.color[0]) (ngon.rotations[0])
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
          (ngon.rotations[n-1])
        + "\n" + (makeNString(n-1) (totalStep - ngon.step[n-2]) ((cx-dx, cy-dy)))

  let totalStep = ngon.step |> List.fold (fun acc x -> acc + x) 0
  let finalx = (fst ngon.center) + (ngon.centerDelta |> List.fold (fun acc x -> acc + (fst x)) 0)
  let finaly = (snd ngon.center) + (ngon.centerDelta |> List.fold (fun acc y -> acc + (snd y)) 0)
  makeNString (ngon.num) (totalStep) ((finalx,finaly))

let clean (ngon) =
  if ngon.color.Length > ngon.num then
    failwith $"Color list too long: must be at most {ngon.num}, the number of ngons"
  elif ngon.step.Length > (ngon.num-1) then
    failwith $"Step list too long: must be at most {ngon.num-1}, one less than the number of ngons"
  elif ngon.centerDelta.Length > (ngon.num-1) then
    failwith $"CenterDelta list too long: must be at most {ngon.num-1}, one less than the number of ngons"
  elif ngon.rotations.Length > ngon.num then
    failwith $"Rotation list too long: must be at most {ngon.num}, the number of ngons"
  elif ngon.step.Length = 0 && ngon.num <> 1 then
    failwith $"Step list must not be empty with more than 1 ngon"
  elif ngon.centerDelta.Length = 0 && ngon.num <> 1 then
    failwith $"CenterDelta list must not be empty with more than 1 ngon"
  elif ngon.color.Length = 0 then
    failwith $"Color list must not be empty"
  elif ngon.rotations.Length = 0 then
    failwith $"Rotation list must not be empty"
  
  let realColors = ngon.color |> extendList ngon.num
  let realSteps = ngon.step |> extendList (ngon.num-1)
  let realDeltas = ngon.centerDelta |> extendList (ngon.num-1)
  let realRotations = ngon.rotations |> extendList ngon.num
  {ngon with step=realSteps; color=realColors; centerDelta=realDeltas; rotations=realRotations}

let allngons (ngonL: ngon list): string =
  ngonL |> 
    List.fold (fun acc ngon -> acc + (multngons (clean ngon)) + "\n") ""



let eval (data: (int * int) * ngon list): string =
  let s = $"viewBox=\"0 0 {fst (fst data)} {snd (fst data)}\" xmlns=\"http://www.w3.org/2000/svg\""
  $"<svg {s}>\n{allngons (snd data)}\n</svg>"