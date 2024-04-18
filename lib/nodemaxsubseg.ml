type data = int
type answer = { sum : int; prefix : int; suffix : int; subseg : int }
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun element -> { sum = element; prefix = element; suffix = element; subseg = element }

let combine : node -> node -> node =
  fun left_node right_node ->

    let max = fun x y -> if x > y then x else y in
    let ans = { sum = (left_node.answer.sum + right_node.answer.sum );
                prefix = max (left_node.answer.prefix) (left_node.answer.sum + right_node.answer.prefix) ;
                suffix = max (right_node.answer.prefix) (right_node.answer.sum + left_node.answer.prefix) ;
                subseg = max (max (left_node.answer.subseg) (right_node.answer.subseg)) (left_node.answer.suffix + right_node.answer.prefix) };
    in
    {
      answer = ans;
      left = left_node.left ;
      right = right_node.right
    }


let to_string : answer -> string = 
  fun element ->
    string_of_int (element.subseg)
