let extract k list =
  let rec aux item = function
    | _ when List.length item = k -> [List.rev item]
    | [] -> []
    | h :: t ->
      let with_h = aux (h::item) t in
      let without_h = aux item t in
      with_h @ without_h
  in
  aux [] list

let () =
  assert (extract 2 [] = []);
  assert (extract 2 ["a"; "b"; "c"; "d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]);
  assert (extract 3 [1; 2; 3; 4; 5] = [[1; 2; 3]; [1; 2; 4;]; [1; 2; 5]; [1; 3; 4]; [1; 3; 5]; [1; 4; 5]; [2; 3; 4]; [2; 3; 5]; [2; 4; 5]; [3; 4; 5]])
  
