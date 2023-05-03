let nucleotide_order = List.mapi (fun ix c -> (c, ix)) ['A'; 'C'; 'G'; 'T']


let pattern_to_number (pattern : string) : int =
    String.fold_left (fun encoding nucleotide -> 4 * encoding + List.assoc nucleotide nucleotide_order) 0 pattern


let () =
    let dna = read_line () in
    let result = pattern_to_number dna in
    print_int result; print_newline ()
