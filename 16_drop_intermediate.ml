let drop list nth =
  let rec aux p acc = function
    | [] -> acc
    | h :: t -> if p = nth then aux 1 acc t else aux (p+1) (h::acc) t
  in
  List.rev (aux 1 [] list)

let () =
  assert (drop [] 2 = []);
  assert (drop [1; 2; 3; 4; 5; 6] 2 = [1; 3; 5;]);
  assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])
