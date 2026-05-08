let compress list =
    let rec aux compressed = function
        | [] -> compressed
        | h :: t -> match compressed with
            | [] -> aux (h::compressed) t
            | x :: _ -> if h = x then aux compressed t else aux (h::compressed) t
    in
    List.rev (aux [] list)

let () =
    assert (compress [] = []);
    assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);
    assert (compress ["a"; "a"] = ["a"]);
    assert (compress ["a"; "b"] = ["a"; "b"]);
    assert (compress ["aa"; "bb"; "bb"; "bb"; "cc"; "cc"; "bb"; "bb"; "bb"; "dd"] = ["aa"; "bb"; "cc"; "bb"; "dd"])
