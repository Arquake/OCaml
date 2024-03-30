type data = int
type answer = { sum : int; prefix : int; suffix : int; subseg : int }
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun element -> { sum = element; prefix = 0; suffix = 0; subseg = 0 }

let combine : node -> node -> node =
fun left_node right_node ->

  let max = if left_node.answer.sum > right_node.answer.sum then left_node.answer.sum else right_node.answer.sum in
  let ans = { sum = (left_node.answer.sum + right_node.answer.sum ); prefix = left_node.answer.prefix ; suffix = right_node.answer.prefix ; subseg = max }; 
  in
  {
    answer = ans;
    left = left_node.left ;
    right = right_node.right
  }


let to_string : answer -> string = 
 fun element ->
  string_of_int (element.subseg)
