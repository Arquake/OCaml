(** Signatures et type fournis. *)

(** Signature du module demandé dans le projet *)
module type QUERY_STRUCTURE = sig
  type data = int
  (** Le type des données sur lesquelles sont faites les requêtes *)

  type answer
  (** Le type des réponses aux requêtes *)

  type tree
  (** Le type Arbre, qui sera défini comme un arbre binaire *)

  val create : data list -> tree
  (** Création de l’arbre *)

  val update : tree -> data -> int -> tree
  (** Mise à jour d’un élément de l'arbre *)

  val query : tree -> int -> int -> answer
  (** [query t l r] est la requête pour obtenir la 
      valeur pour un intervalle [l…r] *)

  val to_string : answer -> string
  (** Pour l'affichage des réponses *)
end

(** Signature utilisée pour les valeurs stockées dans l’arbre *)
module type NODE = sig
  type data = int
  (** Le type des données sur lesquelles sont faites les requêtes *)

  type answer
  (** Le type des réponses aux requêtes *)

  type node = { answer : answer; left : int; right : int }
  (** Les nœuds contiennent des données et les bornes gauche et droite de l’intervalle qu’ils représentent *)

  val create : data -> answer
  (** Création d’une réponse à partir d'une unique valeur *)

  val combine : node -> node -> node
  (** Nœud obtenu par la combinaison de deux autres nœuds *)

  val to_string : answer -> string
  (** Pour l'affichage des réponses *)
end

(** Le type concret des arbres construits pour répondre aux requêtes. 
    Les arbres considérés ne sont jamais vides et contiennent 2^p feuilles. *)
type 'a binary_tree =
  | Leaf of { node : 'a }
  | Node of {
      node : 'a;
      left_child : 'a binary_tree;
      right_child : 'a binary_tree;
    }
