let encode list =
  let rec aux cur acc = function
    | [] | [_] -> cur :: acc
    | a :: (b :: _ as t) ->
       if a = b then
         aux (match cur with
              | (n, x) -> (n+1, x)
           ) acc t
       else
         aux (1, b) (cur::acc) t
  in
  match list with
  | [] -> []
  | h :: _ -> List.rev (aux (1, h) [] list)

let () =
  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);
  assert (encode [1; 1; 22; 3; 3; 4;] = [(2, 1); (1, 22); (2, 3); (1, 4)])
