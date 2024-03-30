type data = int
type answer = int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun element -> 
  { answer = data ; left = 0 ; right = 0 }

let combine : node -> node -> node =
 fun left_node right_node->
  { answer = 0; left = 0; right = 0 }

let to_string : answer -> string = 
 fun _ ->
  ""
