let rotate list k =
  let rec aux p = function
    | a :: t as hole -> if p > 0 then aux (p-1) (List.rev (a :: (List.rev t))) else hole
    | [] -> []
  in
  aux k list

let () =
  assert (rotate [] 3 = []);
  assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])
