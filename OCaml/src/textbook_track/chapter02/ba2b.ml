let nucleotides = ['A'; 'C'; 'G'; 'T']


let read_dna_strings () =
    let rec loop acc =
        try
            let dna = read_line () in
            loop (dna :: acc)
        with End_of_file -> List.rev acc in
    loop []


let calc_hamming_distance (s1 : string) (s2 : string) : int =
    Seq.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 (String.to_seq s1) (String.to_seq s2)


let calc_distance_between_pattern_and_text (text : string) (pattern : string) (k : int) : int =
    let n = String.length text in
    let substrings = Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
    Seq.fold_left (fun acc k_mer -> min acc (calc_hamming_distance k_mer pattern)) k substrings


let distance_between_patterns_and_texts (texts : string list) (pattern : string) (k : int) : int =
    List.fold_left (fun acc text -> acc + calc_distance_between_pattern_and_text text pattern k) 0 texts


let product (xs : 'a Seq.t) (n : int) : 'a list Seq.t =
    let range = Seq.init n Fun.id in
    Seq.fold_left (fun acc _ -> Seq.flat_map (fun ys -> Seq.map (fun x -> x :: ys) xs) acc) (List.to_seq [[]]) range


let calc_median_string (texts : string list) (k : int) : string =
    let string_of_charlist xs = xs |> List.map (String.make 1) |> String.concat "" in
    let patterns = Seq.map string_of_charlist @@ product (List.to_seq nucleotides) k in
    let process_patterns ((minimum_distance, _) as acc) pattern =
        let distance = distance_between_patterns_and_texts texts pattern k in
        if distance < minimum_distance then (distance, pattern) else acc in
    snd @@ Seq.fold_left process_patterns (max_int, "") patterns


let () =
    let k = read_int() in
    let texts = read_dna_strings () in
    let result = calc_median_string texts k in
    print_endline result
