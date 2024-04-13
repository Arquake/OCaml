open Sig

module Make (N : NODE) : QUERY_STRUCTURE = struct
  type tree = N.node Sig.binary_tree
  type data = int
  type answer = N.answer

  let to_string = N.to_string

  (* Création de l’arbre *)
  let create : data list -> tree = fun list ->
    let leave = fun l value ->
      Leaf ({ node = { N.answer = N.create l; left = value; right = value } })
    in
    let rec create_leaves list rest =
      let value = List.length rest in
      match list with
      | [] -> rest
      | h :: [] -> rest @ [leave h value]
      | h :: t -> create_leaves t (rest @ [leave h value])
    in
    let rec create_recursively elements rest =
      match elements with
      | [] -> create_recursively rest []
      | h :: [] -> h
      | h1 :: h2 :: t -> rest @ [Node ({ node = { N.answer = N.combine (h1.node) (h2.node); left_child = h1; right_child = h2} })]
    in
    create_recursively (create_leaves list []) []

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree = fun arbre number position ->
    let rec research_and_replace = function
      | Leaf { node } ->
        if node.left = position && node.right = position then
          Leaf { node = { answer = N.create number; left = position; right = position } }
        else
          Leaf { node }
      | Node { node; left_child; right_child } ->
        if position <= node.left + ((node.right - node.left) / 2) then
          let updated_left_child = research_and_replace left_child in
          Node { node = N.combine updated_left_child.node right_child.node; left_child = updated_left_child; right_child }
        else
          let updated_right_child = research_and_replace right_child in
          Node { node = N.combine left_child.node updated_right_child.node; left_child; right_child = updated_right_child }
    in
    research_and_replace arbre

  let query : tree -> int -> int -> answer = fun tree l r ->
    let rec aux node l r =
      match node with
      | Leaf { node = n } ->
        if n.left >= l && n.right <= r then
          n.answer
        else
          failwith "Outside the range"
      | Node { node = n; left_child = lc; right_child = rc } ->
        if r < n.left || l > n.right then
          failwith "Outside the range"
        else if l <= n.left && r >= n.right then
          n.answer
        else
          let left_node = if l <= n.left + ((n.right - n.left) / 2) then aux lc l r else { node = { answer = N.create 0; left = n.left; right = n.left + ((n.right - n.left) / 2) } } in
          let right_node = if r > n.left + ((n.right - n.left) / 2) then aux rc l r else { node = { answer = N.create 0; left = n.left + ((n.right - n.left) / 2) + 1; right = n.right } } in
          N.combine left_node right_node
    in
    aux tree l r
    
end
