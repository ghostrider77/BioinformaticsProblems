let nucleotide_order = List.mapi (fun ix c -> (c, ix)) ['A'; 'C'; 'G'; 'T']


let pattern_to_number (pattern : string) : int =
    String.fold_left (fun encoding nucleotide -> 4 * encoding + List.assoc nucleotide nucleotide_order) 0 pattern


let compute_frequencies (text : string) (k : int) : int array =
    let text_length = String.length text in
    let size = int_of_float (4.0 ** float k) in
    let frequencies = Array.make size 0 in
    for ix = 0 to text_length - k do
        begin
            let pattern = String.sub text ix k in
            let code = pattern_to_number pattern in
            frequencies.(code) <- frequencies.(code) + 1
        end
    done;
    frequencies


let () =
    let dna = read_line () in
    let k = read_int () in
    let result = compute_frequencies dna k in
    result |> Array.to_seq |> Seq.map string_of_int |> List.of_seq |> String.concat " " |> print_endline
