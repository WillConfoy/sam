module AST

type dist = {
  name: string; 
  param: float * float; 
}

type ngon = {
  num: int;
  numSides: int;
  radius: int;
  center: int * int;
  step: int list;           // size of num-1
  color: string list;       // size of num
  centerDelta: (int * int) list;
  dist: dist;
  granularity: int;
  rotations: float list;
}