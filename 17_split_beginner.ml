let split list first_len =
  let rec aux len acc = function
    | [] -> (List.rev acc, [])
  | h :: t as hole -> if len = first_len then (List.rev acc, hole) else aux (len+1) (h::acc) t
  in
  aux 0 [] list

let () =
  assert (split [] 2 = ([], []));
  assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
  assert (split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []))
