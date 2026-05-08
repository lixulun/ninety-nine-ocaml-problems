type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> acc
    | [b] -> aux 0 (rle count b :: acc) []
    | a :: (b :: _ as t) ->
      if a = b then aux (count+1) acc t
      else aux 1 (rle count a :: acc) t
  in
  List.rev (aux 1 [] list)

let () =
  assert (encode [] = []);
  assert (encode [1; 1; 2; 2; 3; 3; 3; 4; 6; 5; 5] = [Many (2, 1); Many (2, 2); Many (3, 3); One 4; One 6; Many (2, 5)]);
  assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; One "e"])
