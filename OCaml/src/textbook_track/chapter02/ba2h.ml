let calc_hamming_distance (s1 : string) (s2 : string) : int =
    Seq.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 (String.to_seq s1) (String.to_seq s2)


let calc_distance_between_pattern_and_text (text : string) (pattern : string) (k : int) : int =
    let n = String.length text in
    let substrings = Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
    Seq.fold_left (fun acc k_mer -> min acc (calc_hamming_distance k_mer pattern)) k substrings


let distance_between_patterns_and_texts (texts : string list) (pattern : string) (k : int) : int =
    List.fold_left (fun acc text -> acc + calc_distance_between_pattern_and_text text pattern k) 0 texts


let () =
    let pattern = read_line () in
    let k = String.length pattern in
    let texts = String.split_on_char ' ' @@ read_line () in
    let result = distance_between_patterns_and_texts texts pattern k in
    print_int result; print_newline ()
