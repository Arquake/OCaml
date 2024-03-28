module type EXEC = sig
  include Lib.Sig.QUERY_STRUCTURE

  val initial_tree : tree
  val exec : tree -> Cli.command -> tree
end

module Make (M : Lib.Sig.QUERY_STRUCTURE) : EXEC = struct
  include M

  let initial_tree = M.create Cli.config.input

  let print tree =
    let length = List.length Cli.config.input in
    List.iter
      (fun ans -> Printf.printf "%s " (M.to_string ans))
      (List.map (fun i -> M.query tree i i) (List.init length (fun i -> i)));
    flush stdout

  let exec tree =
    let open Cli in
    function
    | Print ->
        print tree;
        print_newline ();
        tree
    | Query (lower, upper) ->
        Printf.printf "%s\n" (M.to_string (M.query tree lower upper));
        flush stdout;
        tree
    | Update (value, index) ->
        let tree = M.update tree value index in
        Printf.printf "input[%d]=%d\n" index value;
        flush stdout;
        tree
end

let _ =
  let open Cli in
  let open Lib.Sig in
  let module Problem =
    (val match config.problem with
         | Sum -> (module Lib.Sum : QUERY_STRUCTURE)
         | MaxOcc -> (module Lib.MaxOcc : QUERY_STRUCTURE)
         | MaxSubSeg -> (module Lib.MaxSubSeg : QUERY_STRUCTURE)
         | _ -> assert false)
  in
  try
    let module E = Make (Problem) in
    ignore (List.fold_left E.exec E.initial_tree Cli.config.commands)
  with Failure msg -> fail msg
