let remove_at idx list =
  let rec aux k left = function
  | [] -> List.rev left
  | h :: t -> if k != idx then aux (k+1) (h::left) t else aux (k+1) left t
  in
  aux 0 [] list

let () =
  assert (remove_at 2 [] = []);
  assert (remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"])
