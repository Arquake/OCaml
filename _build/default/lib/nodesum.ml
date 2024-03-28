type data = int
type answer = int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun _ -> 
  0

let combine : node -> node -> node =
 fun _ _ ->
  { answer = 0; left = 0; right = 0 }

let to_string : answer -> string = 
 fun _ ->
  ""
