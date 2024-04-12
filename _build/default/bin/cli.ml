type error =
  | Incorrect_input_list
  | Unknown_arguments of string list
  | Missing_problem
  | Unknown_problem of string
  | Not_a_number of string
  | Fail of string

let to_string = List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""

let errors = function
  | Incorrect_input_list -> (1, "incorrect input line ")
  | Missing_problem -> (2, "-problem (sum | maxocc | maxsubseg) is mandatory")
  | Unknown_problem p -> (3, "unknown problem " ^ p)
  | Not_a_number n -> (4, "argument " ^ n ^ " is not a valid number")
  | Fail msg -> (5, msg)
  | Unknown_arguments arguments ->
      (10, "unknown arguments: " ^ to_string arguments)

let program =
  if !Sys.interactive then "devoir_de_programmation" else Sys.argv.(0)

let usage =
  program
  ^ " -data \"n1 n2 ...\" -problem (max|maxocc|maxsegsub) (-print | -query \
     lower upper | -update value index)*"
  ^ "\n\t where n1, n2, lower, upper, value, index are integers"
  ^ "\n  -h | --help: prints this message"
  ^ "\n  -data numbers: performs the queries on this initial list of values"
  ^ "\n\
    \  -problem sum: the queries return the sum of the elements in the given \
     interval"
  ^ "\n\
    \  -problem maxocc: the queries return the maximum value and the number of \
     times it occurs in the given interval"
  ^ "\n\
    \  -problem maxsubseg: the queries return the maximum value of the sums of \
     all the sub-segments in the given interval"
  ^ "\n  -query lower upper: a query in the interval [lower..upper]"
  ^ "\n\
    \  -update value index: an update the list with the given value at the \
     given index" ^ "\n  -print: prints the leaves of the tree" ^ "\n"

let error : error -> 'a =
 fun error ->
  let code, msg = errors error in
  Printf.eprintf "%s\n" usage;
  Printf.eprintf "[Error] %s\n" msg;
  flush stderr;
  exit code

let fail msg = error (Fail msg)

type command = Query of int * int | Update of int * int | Print
type problem = Undefined | Sum | MaxOcc | MaxSubSeg
type config = { input : int list; problem : problem; commands : command list }

let to_int_list arg =
  try List.map int_of_string (Str.split (Str.regexp "[ /t/n]") arg)
  with _ -> error Incorrect_input_list

let problem_of_string = function
  | "sum" -> Sum
  | "maxocc" -> MaxOcc
  | "maxsubseg" -> MaxSubSeg
  | p -> error (Unknown_problem p)

let checked_int_of_string number =
  try int_of_string number with _ -> error (Not_a_number number)

let build_command com n1 n2 =
  match com with
  | "-query" -> Query (n1, n2)
  | "-update" -> Update (n1, n2)
  | _ -> assert false

let rec parse config = function
  | ("-h" | "--help") :: _ ->
      print_string usage;
      flush stdout;
      exit 0
  | [] -> config
  | "-data" :: input_list :: rest ->
      parse { config with input = to_int_list input_list } rest
  | "-problem" :: problem :: rest ->
      parse { config with problem = problem_of_string problem } rest
  | "-print" :: rest ->
      parse { config with commands = config.commands @ [ Print ] } rest
  | (("-query" | "-update") as com) :: n1 :: n2 :: rest ->
      let n1 = checked_int_of_string n1 and n2 = checked_int_of_string n2 in
      parse
        { config with commands = config.commands @ [ build_command com n1 n2 ] }
        rest
  | args -> error (Unknown_arguments args)

let config =
  let arguments = List.tl (Array.to_list Sys.argv) in
  let config =
    parse { input = []; problem = Undefined; commands = [] } arguments
  in
  if config.problem = Undefined then error Missing_problem else config
