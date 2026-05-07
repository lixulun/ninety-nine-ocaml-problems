let range a b =
  let rec aux p acc = if p > b then acc else aux (p+1) (p::acc) in
  List.rev (aux a [])

let () =
  assert (range 1 3 = [1; 2; 3]);
  assert (range 7 12 = [7; 8; 9; 10; 11; 12]);
  
