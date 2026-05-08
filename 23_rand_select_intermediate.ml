let rand_select list k =
  let rec aux p selected = function
    | [] -> selected
    | h :: t -> if p < k && Random.bool() then aux (p+1) (h::selected) t else if p >= k then selected  else aux p selected (List.rev (h::(List.rev t)))
  in
  aux 0 [] list


let () =
  assert (rand_select [] 3 = []);
  assert (rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 |> List.length = 3)
