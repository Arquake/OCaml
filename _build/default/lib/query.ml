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
      Leaf({node = {answer = N.create l; left = value; right = value}})
    in
    let rec create_leaves = fun list rest ->
      match list with
      | h::[] -> rest @ [leave h (List.length rest)]
      | h::t -> create_leaves t (rest @ [Leaf(h)])
    in
    let rec create_recursively = fun elements rest ->
      match elements with
      | [] -> create_recursively rest []
      | h::[] -> h
      | h1::h2::t -> rest @ [{node = N.combine h1, h2; right_child = h2 ; left_child = h1}]
    in
    (create_recursively (create_leaves list []) [])

  (* Mise à jour d’un élément de la liste data nouvelle valeur et int l'indice dans le tableau*)
  let update : tree -> data -> int -> tree = fun arbre number position  ->
    let rec research_and_replace =
      function
      | Leaf(x) -> Leaf({node = {answer = N.create number; left = position; right = position}})
      | Node(x) when x.left + ((x.right-x.left) / 2) >= int -> Node({node = N.combine; left_child = (research_and_replace x.left); right_child = (x.right)})
      | Node(x) when x.left + ((x.right-x.left) / 2) <= int -> Node({node = N.combine; left_child = (x.left); right_child = (research_and_replace x.right)})
    in research_and_replace arbre
   

  let query : tree -> int -> int -> answer =
   fun _ _ _ -> 
    N.create 0
end
