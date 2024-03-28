type data = int
type answer = { sum : int; prefix : int; suffix : int; subseg : int }
type node = { answer : answer; left : int; right : int }

let create : data -> answer = 
 fun _ -> { sum = 0; prefix = 0; suffix = 0; subseg = 0 }

let combine : node -> node -> node =
 fun _ _ ->
  {
    answer = { sum = 0 ; prefix = 0 ; suffix = 0 ; subseg = 0 }; 
    left = 0;
    right = 0;
  }

let to_string : answer -> string = 
 fun _ ->
  ""
