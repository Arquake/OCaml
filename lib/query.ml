open Sig

module Make (N : NODE) : QUERY_STRUCTURE = struct
  type tree = N.node Sig.binary_tree
  type data = int
  type answer = N.answer

  let to_string = N.to_string

  (* Création de l’arbre *)
  let create : data list -> tree = 
   fun list->
    let leave = fun l value ->
      let l1 = l.left = value in let l2 = l1.right = value
    in
    let rec create_leaves = fun list rest ->
      match list with
      | h::[] -> rest @ [leave Leaf(h) List.length rest]
      | h::t -> create_leaves t (rest @ [Leaf(h)])
    in
    let rec create_recursively = fun elements rest ->
      match elements with
      | [] -> create_recursively rest []
      | h::[] -> h
      | h1::h2::t when h1.node > h2.node -> rest @ [Node.combine h1, h2]
    in
    (create_recursively (create_leaves list []) [])

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree =
   fun arbre to_update the_update  ->

   

  let query : tree -> int -> int -> answer =
   fun _ _ _ -> 
    N.create 0
end
