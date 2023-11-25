module Parser

open Combinator
open AST

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween pws0 p pws0

(* pnum
 *   Parses a positive integer.
 *)
let pnum: Parser<int> = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

(* pnegnum
 *   Parses a negative integer
 *)
let pnegnum: Parser<int> = pright (pchar '-') pnum |>> (fun d -> -1 * d)

(* pfloat
 *   Parses a float
 *)
let pfloat: Parser<float> = 
  pseq 
    (pleft (pnum <|> pnegnum) (pchar '.'))
    (pnum) 
    (fun (l,r) -> (string l)+"."+(string r) |> float)

(* phexchar
 *   Parses a single hexadecimal character
 *)
let is_hex(c: char) = is_regexp (c.ToString()) @"[A-F]|[a-f]|[0-9]"
let phexchar: Parser<char> =
    cause
        (psat is_hex)
        "is_hex" 

(* phex
 *   Parses a hexidecimal string prepended with "0x"
 *)

let phex: Parser<string> = 
  pseq
    (pchar '#')
    (pmany1 phexchar)
    (fun (prefix, hs) -> stringify (prefix :: hs))

(* ptuple p
 *   Parses tuples of the form "(a1,a2)" where a1 and a2 are of the same type
 *)
let ptuple(p: Parser<'a>): Parser<'a * 'a> = 
  pbetween 
    (pchar '(') 
    (pseq (pleft (p) (pchar ',')) (p) (fun t -> t)) 
    (pchar ')')

(* pcommaseq p
 *   Parses a comma seperated sequnce of the form "a1" or "a1,a2,a3,...", where
 *   each a_i is of type 'a. 
 *) //FIX THISSSSSSSSSSSS
let pcommaseq(p: Parser<'a>): Parser<'a list> =
  pseq
    (pmany0 (pleft p (pchar ',')))
    p
    (fun (AS, A) -> AS@[A])

(* plist p
 *   Parses a comma seperated list of the form "[a1]" or "[a1,a2,a3,...]", 
 *   where each a_i is of type 'a. 
 *)
let plist(p: Parser<'a>): Parser<'a list> =
  pbetween
    (pchar '[')
    (pcommaseq p)
    (pchar ']')


(* pfield s p
 *    Parses a field of a polygon
 *)
let pfield(s: string)(p: Parser<'a>): Parser<'a> =
  pright
    (pstr s)
    (pleft (p) (pchar ','))

(* psides
 *   Parses the number of sides of the polygon. Input is of the form
 *   "n-gon with radius" where n is a positive integer parsed by pnum.  
 *)
let psides: Parser<int> = pleft pnum (pstr "-gon with radius")

(* pcenter
 *   Parses the center of the polygon. Input is of the form "and center(n1,n2)," 
 *   where n1 and n2 is a positive integers parsed by pnum.  
 *)
let pcenter: Parser<int * int> = pfield ("and center") (pad (ptuple pnum))

(* pstep
 *   Parses the step of the polygon. Input is of the form "step[n1]," or 
 *   "step[n1,n2,n3,...]," where each n_i is a positive intger parsed by pnum
 *)
let pstep: Parser<int list> = 
  pfield ("step") (pad ((plist pnum) <|> (pstr "[]" |>> (fun _ -> []))))

(* pcolor
 *   Parses the color list of the polygon. Input is of the form 
 *   "color[0xFFFFFF]" or "color[0xFFFFFF,0xFF00AA,...]" where each hex string
 *   is the color of a single polygon
 *)
let pcolor: Parser<string list> = pfield ("color") (pad (plist phex))

(* pcenterDelta
 *   Parses how the center of the polygon changes. Input is of the form
 *   "centerDelta(n1,n2)," where n1 and n2 are positive integers parsed by pnum. 
 *)
let pcenterDelta: Parser<(int * int) list> = 
  pfield ("centerDelta") (pad (plist (ptuple (pnum <|> pnegnum))))

(* pdist
 *   Parses the distribution field of the polygon.
 *)
let pname: Parser<string> = 
  (pstr "normal") <|> (pstr "uniform") <|> (pstr "laplace") <|> (pstr "fish")
let pdist: Parser<dist> = 
  pseq
    (pad pname)
    (pad (ptuple pfloat))
    (fun (n,t) -> {name = n; param = t})

(* pgran
 *   Parses the granularity field, which consists of "and granularity" followed
 *   by an integer.
 *)
let pgran: Parser<int> = pright (pad (pstr "and granularity")) pnum

let pcanvas = 
  pbetween
    (pstr "[<canvas") 
    (pad (ptuple pnum))
    (pstr ">]")

let prot = pfield "rotation" (pad (plist pfloat))


let pfirst: Parser<int * int * int * (int * int)> =
  pseq
    (pseq
      (pleft pnum pws1)
      (pad psides)
      (fun i -> i))
    (pseq
      (pad pnum)
      (pad pcenter)
      (fun i -> i))
    (fun ((a, b), (c, (x, y))) -> (a,b,c,(x,y)))

let psecond: Parser<int list * float list * string list * (int * int) list> =
  pseq
    (pseq
      (pad pstep)
      (pad prot)
      (fun i -> i))
    (pseq
      (pad pcolor)
      (pad pcenterDelta)
      (fun i -> i))
    (fun ((ns,rots),(colors,deltas)) -> (ns,rots,colors,deltas))

let pthird: Parser<dist * int> =
  pseq
    (pad pdist)
    (pad pgran)
    (fun i -> i)


let pngon: Parser<ngon> = 
  pseq
    (pseq
      pfirst
      psecond
      (fun i -> i))
    pthird
    (fun (((num,sides,rad,cent),(rs,rots,cs,cents)),(j,k)) ->
      {num = num;
      numSides = sides;
      radius = rad;
      center = cent;
      step = rs;
      rotations = rots;
      color = cs;
      centerDelta = cents;
      dist = j;
      granularity = k})

let ngonlist = pmany1 ((pleft pngon (pmany1 (pchar '\n'))) <|> pngon)

// let grammar = pleft expr peof
let expr =
  pseq
    (pleft (pad pcanvas) (pmany0 pnl))
    ngonlist
    (fun x -> x)

let grammar = pleft expr peof

// let grammar: Parser<(int * int) * ngon list> =
//   pseq
//     (pseq
//       (pleft (pad pcanvas) (pmany0 pnl))
//       expr
//       (fun x -> x))
//     peof
//     (fun (x,_) -> x)

let parse(s: string): ((int * int) * ngon list) option =
    let input = prepare s
    match grammar input with
        | Success(ast,_) -> Some ast
        | Failure(pos,rule) ->
            printfn "Invalid Expression"
            let msg =
                sprintf
                    "Cannot parse input at position %d in rule '%s':" pos rule
            let diag = diagnosticMessage 20 (pos - 1) s msg
            printf "%s" diag
            None


// let grammar: Parser<ngon list> = pleft (pmany1 pngon) peof