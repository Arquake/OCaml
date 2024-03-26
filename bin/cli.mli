val fail : string -> 'a

type command = Query of int * int | Update of int * int | Print

type problem = Undefined | Sum | MaxOcc | MaxSubSeg

type config = {
  input : int list;
  problem : problem;
  commands : command list;
}

val config : config
