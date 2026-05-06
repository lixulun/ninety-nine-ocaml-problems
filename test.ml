let print_help () =
  print_endline "Lixulun's Ninety-Nine Ocaml Problems solution";
  print_endline "Ref: https://ocaml.org/exercises";
  print_endline "";
  print_endline "Examples:";
  print_endline "";
  print_endline "  $ ocaml test.ml  # run all tests";
  print_endline "  $ ocaml test.ml list  # list all exercises";
  print_endline "  $ ocaml test.ml 01_xxx_xxx.ml # run specified exercise";
  print_endline "  $ ocaml test.ml 01_xxx.ml 02_xxx.ml ... #  run specified exercises";
  print_endline "  $ ocaml test.ml help / -h / --help  # print this"

let is_exercise_file name =
  Option.is_some (int_of_string_opt (String.sub name 0 2)) && String.ends_with ~suffix:".ml" name
  
let all_test_files () =
  let exercise_file_filter name = if is_exercise_file name then Some name else None in
  Sys.readdir "."
  |> Array.to_seq
  |> List.of_seq
  |> List.filter_map exercise_file_filter
  |> List.sort String.compare

let run_tests args =
  let all_files =
    all_test_files() in    
  let to_file_name arg =
    List.fold_left (fun found try_file -> if Option.is_some found then found else (if try_file = arg then Some try_file else None)) None all_files in
  let rec aux = function
    | [] -> ()
    | h :: t -> (
      let command = "ocaml " ^ h  in
      let result = (print_endline ("running " ^ command); Sys.command command) in
      if result = 0 then (print_endline "test passed"; aux t) else (print_endline "test failed"; aux t))
  in
  aux (List.filter (fun arg -> Option.is_some (to_file_name arg)) args)
  

let run_all_tests () =
  run_tests (all_test_files())

let list_all () =
  let rec aux = function
  | [] -> ()
  | h :: t -> print_endline h; aux t
  in
  aux (all_test_files())

let () =
  let argv = List.of_seq (Array.to_seq Sys.argv) in
  if Array.length Sys.argv = 1 then
    run_all_tests()
  else
  match argv with
  | _ :: first :: [] -> (
    match first with
    | "help" | "-h" | "--help" -> print_help()
    | "list" -> list_all()
    | _ -> run_tests [first]
  )
  | _ :: (_ :: _ as t) -> run_tests t
  | [_] | [] -> run_all_tests()
    
