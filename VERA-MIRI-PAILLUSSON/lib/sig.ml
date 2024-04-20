module type QUERY_STRUCTURE = sig
  type data = int
  type answer
  type tree

  val create : data list -> tree
  val update : tree -> data -> int -> tree
  val query : tree -> int -> int -> answer
  val to_string : answer -> string
end

module type NODE = sig
  type data = int
  type answer
  type node = { answer : answer; left : int; right : int }

  val create : data -> answer
  val combine : node -> node -> node
  val to_string : answer -> string
end

type 'a binary_tree =
  | Leaf of { node : 'a }
  | Node of {
      node : 'a;
      left_child : 'a binary_tree;
      right_child : 'a binary_tree;
    }
