open Sig

module Make (N : NODE) : QUERY_STRUCTURE = struct
  type tree = N.node Sig.binary_tree
  type data = int
  type answer = N.answer

  let to_string = N.to_string

  (* Création de l’arbre *)
  let create : data list -> tree = 
   fun _ ->
    Leaf { node = { answer = N.create 0 ; left = 0 ; right = 0 } }

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree =
   fun _ _ _  ->
    Leaf { node = { answer = N.create 0 ; left = 0 ; right = 0 } }

  let query : tree -> int -> int -> answer =
   fun _ _ _ -> 
    N.create 0
end
