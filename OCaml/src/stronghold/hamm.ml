let calc_hamming_distance s1 s2 =
    Seq.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 (String.to_seq s1) (String.to_seq s2)


let () =
    let s1 = read_line () in
    let s2 = read_line () in
    let result = calc_hamming_distance s1 s2 in
    print_int result; print_newline ()
