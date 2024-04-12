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
      | h1::h2::t -> rest @ [{node = N.combine h1, h2 ; left_child = h1; right_child = h2}]
    (create_recursively (create_leaves list []) [])

  (* Mise à jour d’un élément de la liste data nouvelle valeur et int l'indice dans le tableau*)
  let update : tree -> data -> int -> tree = fun arbre number position  ->
    let rec research_and_replace =
      function
      | Leaf(x) -> 
        if x.left = position && x.right = position then
          Leaf({node = {answer = N.create number; left = position; right = position}})
        else
          Leaf(x)
      | Node(x) when x.left + ((x.right-x.left) / 2) >= int -> Node({node = N.combine; left_child = (research_and_replace x.left); right_child = (x.right)})
      | Node(x) when x.left + ((x.right-x.left) / 2) <= int -> Node({node = N.combine; left_child = (x.left); right_child = (research_and_replace x.right)})
    in research_and_replace arbre
   

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
            n.answer  (* this only happens when the query gods smile upon us*)
            
          else (*this happens when the query gods aren't satisfied with our blood sacrifises *)
           let left_node = if l <= n.left + ((n.right - n.left) / 2) then aux lc l r else { answer = N.create 0; left = n.left; right = n.left + ((n.right - n.left) / 2) } in
           let right_node = if r > n.left + ((n.right - n.left) / 2) then aux rc l r else { answer = N.create 0; left = n.left + ((n.right - n.left) / 2) + 1; right = n.right } in
            (*i'm pretty sure these are right*)
           (N.combine left_node right_node).answer
      in
      aux tree l r
    
end
