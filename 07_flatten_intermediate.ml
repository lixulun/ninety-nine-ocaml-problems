type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
    let rec aux acc = function
        | [] -> acc
        | One h :: t -> aux (h :: acc) t
        | Many h :: t -> aux (aux acc h) t
    in
    List.rev ( aux [] list)

let () =
    assert (flatten [One "a"; Many [One "b"; Many [One "c" ; One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"])
