let permutation list =
  let rec rand_select selected = function
    | [] -> selected
    | h :: t -> if Random.bool () then rand_select (h::selected) t else rand_select selected (List.rev (h::(List.rev t)))
  in
  rand_select [] list

let () =
  assert (permutation [] = []);
  assert (let res = permutation [1; 2; 3] in List.length res = 3 && List.sort Int.compare [1; 2; 3] = List.sort Int.compare res);
  assert (let list =  ["a"; "b"; "c"; "d"; "e"; "f"] in let res = permutation list in List.length res = 6 && List.sort String.compare list = List.sort String.compare res)
