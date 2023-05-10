module StringSet = Set.Make(String)

let nucleotides = ['A'; 'C'; 'G'; 'T']
let nucleotide_complements = [('A', 'T'); ('C', 'G'); ('T', 'A'); ('G', 'C')]
let nucleotide_order = List.mapi (fun ix c -> (c, ix)) nucleotides


let calc_reverse_complement (dna : string) : string =
    let open Batteries in
    String.(dna |> map (fun nucleotide -> List.assoc nucleotide nucleotide_complements) |> rev)


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


let calc_immediate_neighborhood (dna : string) : StringSet.t =
    let replace (ix: int) (c : char) : string =
        let text = String.to_bytes dna in
        Bytes.set text ix c;
        Bytes.to_string text in
    let change_char ix nucleotide =
        List.(nucleotides |> filter_map (fun n -> if n = nucleotide then None else Some (replace ix n)) |> to_seq) in
    Seq.fold_left (fun acc (ix, c) -> StringSet.add_seq (change_char ix c) acc) StringSet.empty @@ String.to_seqi dna


let generate_neighborhood (dna : string) (d : int) : StringSet.t =
    let get_neighbors (neighborhood : StringSet.t) : StringSet.t =
        StringSet.fold (fun pattern acc -> StringSet.union acc (calc_immediate_neighborhood pattern)) neighborhood
            StringSet.empty in
    let range = (Seq.init d (fun ix -> ix)) in
    Seq.fold_left (fun acc _ -> StringSet.union acc (get_neighbors acc)) (StringSet.singleton dna) range


let calc_frequencies_for_approximate_occurrences(text: string) (k: int) (d : int) : int array =
    let text_length = String.length text in
    let frequencies = Array.make (integer_power 4 k) 0 in
    let update_frequencies pattern =
        let code = pattern_to_number pattern in
        frequencies.(code) <- frequencies.(code) + 1 in
    for ix = 0 to text_length - k do
        begin
            let exact_pattern = String.sub text ix k in
            let neighbors = generate_neighborhood exact_pattern d in
            let reverse_neighbors = StringSet.map calc_reverse_complement neighbors in
            StringSet.iter update_frequencies neighbors;
            StringSet.iter update_frequencies reverse_neighbors;
        end
    done;
    frequencies


let most_frequent_approximate_k_mers_with_reverse_complements (text : string) (k : int) (d : int) : string list =
    let frequencies = calc_frequencies_for_approximate_occurrences text k d in
    let max_occurrence = Array.fold_left max 0 frequencies in
    frequencies
        |> Array.to_seqi
        |> Seq.filter_map (fun (ix, freq) -> if freq = max_occurrence then Some (number_to_pattern ix k) else None)
        |> List.of_seq


let () =
    let text = read_line () in
    let k, d = Scanf.sscanf (read_line ()) "%d %d" (fun k d -> (k, d)) in
    let result = most_frequent_approximate_k_mers_with_reverse_complements text k d in
    result |> String.concat " " |> print_endline
