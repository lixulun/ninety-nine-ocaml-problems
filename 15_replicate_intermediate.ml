let replicate list k =
  let rec aux p acc = function
  | [] -> acc
  | h :: t as ht -> if p = 0 then aux k acc t else aux (p-1) (h::acc) ht
  in
  List.rev (aux k [] list)

let () =
  assert (replicate [] 5 = []);
  assert (replicate [1; 2; 3] 4 = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3]);
  assert (replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])
