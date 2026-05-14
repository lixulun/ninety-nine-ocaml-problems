let extract k list =
  let rec aux acc item list2 = match list2, item with
  | [], _ when List.length item = k -> List.rev item :: acc
  | [], _ -> acc
  | h :: t, _ :: rest_item when List.length item = k -> aux (List.rev item :: acc) rest_item t
  | h :: t, _ :: rest_item -> aux acc rest_item t @ aux acc item t @ aux acc (h::item) t
  | h :: t, [] -> aux acc item t @ aux acc (h::item) t
  in
  List.rev (aux [] [] list)
  

let () =
  assert (extract 2 [] = []);
  assert (extract 2 ["a"; "b"; "c"; "d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]);
  assert (extract 3 [1; 2; 3; 4; 5] = [[1; 2; 3]; [1; 2; 4;]; [1; 2; 5]; [1; 3; 4]; [1; 3; 5]; [1; 4; 5]; [2; 3; 4]; [2; 3; 5]; [2; 4; 5]; [3; 4; 5]])
  
