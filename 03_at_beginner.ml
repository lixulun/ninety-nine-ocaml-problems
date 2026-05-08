let rec at idx lst =
    match (idx, lst) with
    | (_, []) -> None
    | (0, [e]) -> Some e
    | (a, _ :: t) -> at (a-1) t

let () =
    assert (at 2 [] = None);
    assert (at 0 [1] = Some 1);
    assert (at 1 [0; 1] = Some 1);
    assert (at 10 [0; 1; 2; 3; 4; 5] = None);
    assert (at 4 ["H"; "e"; "l"; "l"; "o"] = Some "o")
