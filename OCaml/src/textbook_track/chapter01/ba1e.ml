let nucleotides = ['A'; 'C'; 'G'; 'T']

let nucleotide_order = List.mapi (fun ix nucleotide -> (nucleotide, ix)) nucleotides


let integer_power (n : int) (k : int) : int =
    Seq.(fold_left (fun acc _ -> n * acc) 1 @@ init k (fun _ -> 0))


let pattern_to_number (pattern : string) : int =
    String.fold_left (fun encoding nucleotide -> 4 * encoding + List.assoc nucleotide nucleotide_order) 0 pattern


let number_to_pattern (encoding : int) (k : int) : string =
    let rec loop pattern number l =
        if l = k then pattern |> List.to_seq |> String.of_seq
        else
            let number' = number / 4 in
            let remainder = number mod 4 in
            loop ((List.nth nucleotides remainder) :: pattern) number' (l + 1) in
    loop [] encoding 0


let compute_frequencies (text : string) (k : int) : int array =
    let text_length = String.length text in
    let size = integer_power 4 k in
    let frequencies = Array.make size 0 in
    for ix = 0 to text_length - k do
        begin
            let pattern = String.sub text ix k in
            let code = pattern_to_number pattern in
            frequencies.(code) <- frequencies.(code) + 1
        end
    done;
    frequencies


let collect_clump_forming_k_mers (clump_array : bool array) (k : int) : string list =
    clump_array |> Array.to_seqi
                |> Seq.filter_map (fun (ix, is_clump) -> if is_clump then Some (number_to_pattern ix k) else None)
                |> List.of_seq


let find_clumps_in_text (genome : string) (k : int) (l : int) (t : int) : string list =
    let length = String.length genome in
    let clump_array = Array.make (integer_power 4 k) false in
    let frequencies = compute_frequencies (String.sub genome 0 l) k in
    Array.iteri (fun ix freq -> if freq >= t then clump_array.(ix) <- true) frequencies;

    for ix = 1 to length - l do
        begin
            let start_k_mer = String.sub genome (ix - 1) k in
            let start_code = pattern_to_number start_k_mer in
            frequencies.(start_code) <- frequencies.(start_code) - 1;
            let end_k_mer = String.sub genome (ix + l - k) k in
            let end_code = pattern_to_number end_k_mer in
            frequencies.(end_code) <- frequencies.(end_code) + 1;
            if frequencies.(end_code) >= t then clump_array.(end_code) <- true;
        end
    done;

    collect_clump_forming_k_mers clump_array k


let () =
    let genome = read_line () in
    let k, l, t = Scanf.sscanf (read_line ()) "%d %d %d" (fun k l t -> (k, l, t)) in
    let result = find_clumps_in_text genome k l t in
    result |> String.concat " " |> print_endline
