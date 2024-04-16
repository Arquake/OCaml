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
      | _ -> create_recursively rest []
      
    in
    create_recursively (create_leaves list []) []

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree = fun arbre number position ->
    let get_node = 
      function
      | Node(n) -> n.node
      | Leaf(n) -> n.node
    in
    let rec research_and_replace = function
      | Leaf ({node={N.answer;left;right}}) ->
        if left= position && right = position then
          Leaf ({ node = { N.answer = N.create number; left = position; right = position } })
        else
          Leaf ({node={N.answer;left;right}})
      | Node ({ node={N.answer = _;left;right}; left_child; right_child })->
        (* Si la position rechercher est à gauche on se déplace à gauche *)
        if position <= left + ((right - left) / 2) then
          let updated_left_child = research_and_replace left_child in
          Node ({ node = N.combine (get_node updated_left_child) (get_node right_child); left_child = updated_left_child; right_child })
        else
          (* Si la position rechercher est à droite on se déplace à droite *)
          let updated_right_child = research_and_replace right_child in
          Node ({ node = N.combine (get_node left_child) (get_node updated_right_child); left_child; right_child = updated_right_child })
    in
    research_and_replace arbre

    let query : tree -> int -> int -> answer = fun tree left_border right_border ->
      let get_middle : N.node -> int = fun n -> 
        (* On déduit la position du noeud actuel *)
        (* donne la position minimum à droite *)
        n.left + ((n.right - n.left) / 2) 
      in
      let rec aux : tree -> int -> int -> N.node = fun node l r ->
        match node with
        (* Si une feuille est trouvée on récupère la réponse et la renvoit à la fonction ayant appelé celle-ci *)
        | Leaf { node } -> node

        (* Si la node est comprise entre les bordures droite et gauche strictement *)
        | Node { node; left_child= _; right_child = _ } when l = node.left && r = node.right -> node (* this only happens when the query gods smile upon us*)

        (* Si la node est comprise entre les bordures droite inférieur au max et gauche *)
        | Node { node; left_child = _; right_child = _ } when l = node.left && r > node.right -> N.combine node (aux tree(node.right + 1) r)

        (* Si la node est hors de la range rechercher on renvoit une erreur *)
        | Node { node = {N.answer = _ ; left ;right}; left_child = _; right_child = _ } when r < left || l > right -> raise (Invalid_argument "Outside the range")

        (*
           this happens when the query gods aren't satisfied with our blood sacrifises
        *)


        (* Si une Node est trouvée *)
        | Node { node; left_child; right_child = _ } when l <= get_middle node -> aux left_child l r

        (* Si une Node est trouvée *)
        | Node { node = _; left_child = _; right_child } -> aux right_child l r(*query gods are obviously trying to make me kill myself*)
      in
      (aux tree left_border right_border).answer
    
    
end
