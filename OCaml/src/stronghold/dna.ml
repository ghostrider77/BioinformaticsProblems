let nucleotides = ['A'; 'C'; 'G'; 'T']


let count_nucleotides (dna : string) : int list =
    let counter = Hashtbl.create 4 in
    let get_or_else table key default = match Hashtbl.find_opt table key with
        | None -> default
        | Some x -> x in
    let update_counter nucleotide =
        let count = get_or_else counter nucleotide 0 in
        Hashtbl.replace counter nucleotide (count + 1) in
    String.iter update_counter dna;
    List.map (fun nucleotide -> get_or_else counter nucleotide 0) nucleotides


let () =
    let dna = read_line () in
    let result = count_nucleotides dna in
    result |> List.map string_of_int |> String.concat " " |> print_endline
