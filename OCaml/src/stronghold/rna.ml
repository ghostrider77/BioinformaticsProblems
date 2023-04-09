let transcribe_dna (dna : string) : string =
    String.map (fun nucleotide -> if nucleotide = 'T' then 'U' else nucleotide) dna


let () =
    let dna = read_line () in
    let result = transcribe_dna dna in
    print_endline result
