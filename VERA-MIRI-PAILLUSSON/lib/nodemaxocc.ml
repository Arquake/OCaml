type data = int
type answer = int * int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun element -> 
  (element,1)

let combine : node -> node -> node =
 fun n1 n2 ->
  match n1.answer with
  | (e1,_) when fst n2.answer > e1 -> {answer = (fst n2.answer,snd n2.answer); left = n1.left; right = n2.right }
  | (e1,occ) when fst n2.answer = e1 -> {answer = (e1,snd n2.answer + occ); left = n1.left; right = n2.right }
  | ele -> {answer = ele; left = n1.left; right = n2.right }

let to_string : answer -> string = 
 function
 | (first,second) -> "(" ^ (string_of_int first) ^ ", " ^ (string_of_int second) ^ ")" 
