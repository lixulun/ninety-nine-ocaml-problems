let length list =
    let rec aux acc = function
        | [] -> acc
        | _ :: t -> aux (acc+1) t
    in
    aux 0 list

let () =
    assert (length [] = 0);
    assert (length [1; 2; 3] = 3);
    assert (length ["hello"; "world"] = 2)
