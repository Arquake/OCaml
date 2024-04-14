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
      | Leaf(h1) :: Leaf(h2) :: t -> create_recursively t (rest @ [Node ({node = N.combine (h1.node) (h2.node); left_child = Leaf(h1); right_child = Leaf(h2)})])
      | Node(h1) :: Node(h2) :: t -> create_recursively t (rest @ [Node ({node = N.combine (h1.node) (h2.node); left_child = Node(h1); right_child = Node(h2)})])
      | h :: [] -> h
      | [] -> create_recursively rest []
      
    in
    create_recursively (create_leaves list []) []

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree = fun arbre number position ->
    let rec research_and_replace = function
      | Leaf ({node={N.answer;left;right}}) ->
        if left= position && right = position then
          Leaf ({ node = { N.answer = N.create number; left = position; right = position } })
        else
          Leaf ({node={N.answer;left;right}})
      | Node ({ node={N.answer;left;right}; left_child; right_child })->
        if position <= left + ((right - left) / 2) then
          let updated_left_child = research_and_replace left_child in
          Node ({ node = N.combine updated_left_child.node right_child.node; left_child = updated_left_child; right_child })
        else
          let updated_right_child = research_and_replace right_child in
          Node ({ node = N.combine left_child.node updated_right_child.node; left_child; right_child = updated_right_child })
    in
    research_and_replace arbre

    let query : tree -> int -> int -> answer = fun tree l r ->
      let rec aux node l r =
        match node with
        | Leaf { node = { N.answer; left; right } } ->
          if left >= l && right <= r then
            answer
          else
            failwith "Outside the range"
        | Node { node = { N.answer; left; right }; left_child; right_child } ->
          if r < left || l > right then
            failwith "Outside the range"
          else if l <= left && r >= right then
            answer
          else
            let middle = left + ((right - left) / 2) in
            let left_node = if r <= middle then aux left_child l r else { answer = N.create 0; left = left; right = middle } in
            let right_node = if l > middle then aux right_child l r else { answer = N.create 0; left = middle + 1; right = right } in
            N.combine left_node right_node
      in
      aux tree l r
    
    
end
