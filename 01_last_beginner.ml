let rec last = function
  | [] -> None
  | [a] -> Some a
  | _ :: t -> last t

let () =
  assert (last [] = None);
  assert (last ["a" ; "b" ; "c" ; "d"] = Some "d");
