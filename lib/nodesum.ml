type data = int
type answer = int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun element -> 
  element

let combine : node -> node -> node =
 fun left_node right_node->
  { answer = left_node.answer + right_node.answer; left = left_node.left; right = right_node.right }

let to_string : answer -> string = 
 fun element ->
  string_of_int element
