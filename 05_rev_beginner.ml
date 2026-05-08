let rev list =
    let rec aux res = function
        | [] -> res
        | x :: t -> aux (x :: res) t
    in
    aux [] list

let () =
    assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);
    assert (rev [] = []);
    assert (rev [1; 2; 3; 4; 5] = [5; 4; 3; 2; 1])
