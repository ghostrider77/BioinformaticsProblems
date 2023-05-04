let calc_hamming_distance (s1 : string) (s2 : string) : int =
    Seq.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 (String.to_seq s1) (String.to_seq s2)


let find_approximate_pattern_occurrences (text : string) (pattern : string) (d : int) : int list =
    let text_length = String.length text in
    let pattern_length = String.length pattern in
    let is_approximate_pattern_index ix =
        let substring = String.sub text ix pattern_length in
        calc_hamming_distance substring pattern <= d in
    let indices = Seq.init (text_length - pattern_length) (fun ix -> ix) in
    indices |> Seq.filter is_approximate_pattern_index |> List.of_seq


let () =
    let pattern = read_line () in
    let text = read_line () in
    let d = read_int () in
    let result = find_approximate_pattern_occurrences text pattern d in
    result |> List.map string_of_int |> String.concat " " |> print_endline
