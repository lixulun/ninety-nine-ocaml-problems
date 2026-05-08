let slice list i k =
  let rec aux p acc = function
  | [] -> acc
  | a :: t -> if p >= i && p <= k then aux (p+1) (a::acc) t else aux (p+1) acc t
  in
  List.rev (aux 0 [] list)

let () =
  assert (slice [] 2 4 = []);
  assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"])
