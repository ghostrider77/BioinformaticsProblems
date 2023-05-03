open Batteries

let nucleotide_complements = [('A', 'T'); ('C', 'G'); ('T', 'A'); ('G', 'C')]


let calc_reverse_complement (dna : string) : string =
    String.(dna |> map (fun nucleotide -> List.assoc nucleotide nucleotide_complements) |> rev)


let () =
    let dna = read_line () in
    let result = calc_reverse_complement dna in
    print_endline result
