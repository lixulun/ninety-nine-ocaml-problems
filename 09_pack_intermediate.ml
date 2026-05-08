let pack list =
  let rec aux cur acc  = function
    | a :: (b :: _ as t) ->
       (if a = b then
         aux (b::cur) acc t
       else
         aux [b] (cur::acc) t)
    | _ -> cur::acc
  in
  let r = match list with
    | [] -> []
    | h :: _ -> aux [] [] (h :: list)
  in
  List.rev r

let () =
  assert (pack [] = []);
  assert (pack [1; 2; 1; 3; 3; 2] = [[1]; [2]; [1]; [3; 3]; [2]]);
  assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]);
  assert (pack [1; 1; 1; 1; 1] = [[1; 1; 1; 1; 1]])
