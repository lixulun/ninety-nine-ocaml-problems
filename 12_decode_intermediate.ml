type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x::acc) t
    | Many (n, x) :: t -> if n = 1 then aux (x::acc) t else aux (x::acc) (Many (n-1, x)::t)
  in
  List.rev (aux [] list)

let () =
  assert (decode [] = []);
  assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
