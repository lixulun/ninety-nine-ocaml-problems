let lotto_select k max =
  let rec aux t acc =
  let rand = (Random.int max) + 1 in
  if t = k then acc else aux (t+1) (rand::acc)
  in
  aux 0 []

let max list =
  let rec aux m = function
    | [] -> m
    | h :: t -> if h > m then aux h t else aux m t
  in
  aux Int.min_int list

let () =
  assert (lotto_select 0 10 = []);
  assert (let selected = lotto_select 8 10  in List.length selected = 8 && max selected <= 10)
