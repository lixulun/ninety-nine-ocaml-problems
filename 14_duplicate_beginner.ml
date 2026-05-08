let duplicate list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (h::h::acc) t
  in
  List.rev (aux [] list)

let () =
  assert (duplicate [] = []);
  assert (duplicate [1; 2; 3] = [1; 1; 2; 2; 3; 3;]);
  assert (duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
