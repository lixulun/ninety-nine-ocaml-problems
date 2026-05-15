let group list groups_size =
  let rec group_size_helper acc = function
    | [] -> acc
    | h :: t -> group_size_helper (acc + h) t
  in
  let target_length = group_size_helper 0 groups_size in
  let flatten_length lst = List.length (List.flatten lst) in
  let rec aux item restrict ks list =
    match ks, list with
    | _, _ when flatten_length item = target_length -> [List.rev item]
    | [], _ -> [] 
    | k :: rest_ks, h::t ->
      (match item with
      | [] ->
	let with_h = aux ([h]::item) restrict ks t in
	let without_h = aux item (h::restrict) ks t in
	with_h @ without_h
      | item_h::item_t ->
	let with_h = aux ((List.rev (h::item_h))::item_t) restrict ks t in
	let without_h = aux item (h::restrict) ks t in
	if List.length item_h = k then aux ([]::item) [] rest_ks (List.rev restrict @ list)
	else with_h @ without_h
      )
    | k:: rest_ks, [] ->
      (match item with
      | [] -> []
      | item_h :: item_t ->
	if List.length item_h = k then aux ([]::item) [] rest_ks (List.rev restrict)
	else []
      )
  in
  aux [] [] groups_size list
