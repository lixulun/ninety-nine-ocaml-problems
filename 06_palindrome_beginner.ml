let palindrome list =
    let rec aux rev = function
        | [] -> rev = list
        | h :: t -> aux (h::rev) t
    in
    aux [] list

let () =
    assert (palindrome [] = true);
    assert (palindrome [1] = true);
    assert (palindrome ["x"; "a"; "m"; "a"; "x"] = true);
    assert (palindrome ["a"; "1"] = false)
