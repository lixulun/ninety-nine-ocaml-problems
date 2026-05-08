type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let rec aux cur acc = function
    | [] | [_] -> cur::acc
    | a :: (b :: _ as t) ->
       if a = b then
         match cur with
         | One x -> aux (Many (2, x)) acc t
         | Many (n, x) -> aux (Many (n+1, x)) acc t
       else
         aux (One b) (cur::acc) t
  in
  match list with
  | [] -> []
  | h :: _ -> List.rev (aux (One h) [] list)

let () =
  assert (encode [] = []);
  assert (encode [1; 1; 1; 2; 3; 3;] = [Many (3, 1); One 2; Many (2, 3)]);
  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
