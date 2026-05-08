let insert_at item at list =
  let rec aux p left = function
    | [] -> List.rev left
    | h :: t -> if p = at then aux (p+1) (h::item::left) t else aux (p+1) (h::left) t
  in
  aux 0 [] list

let () =
  assert (insert_at "a" 1 [] = []);
  assert (insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"])
