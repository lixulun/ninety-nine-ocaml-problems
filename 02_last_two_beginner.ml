let rec last_two = function
    | []
    | [_] -> None
    | [a; b] -> Some (a, b)
    | _ :: t -> last_two t

let print_last_two = function
    | None -> Printf.printf "None\n"
    | Some (a, b) -> Printf.printf "(%d, %d)\n" a b


let () =
    assert (last_two [] = None);
    assert (last_two [1] = None);
    assert (last_two ["hello"; "world"] = Some ("hello", "world"));
    assert (last_two [1.2; 2.3; 3.4; 4.5] = Some (3.4, 4.5))

