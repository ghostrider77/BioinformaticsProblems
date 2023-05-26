module StringSet = Set.Make(String)

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


let motif_enumeration (texts : string list) (k : int) (d : int) : StringSet.t =
    let get_substrings text =
        let n = String.length text in
        Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
    let does_k_mer_occur_approximately_in_text k_mer text =
        let substrings = get_substrings text in
        Seq.exists (fun substring -> calc_hamming_distance substring k_mer <= d) substrings in
    let all_texts_approximately_contain_pattern k_mer =
        List.for_all (does_k_mer_occur_approximately_in_text k_mer) texts in
    let rec loop acc = function
        | [] -> acc
        | text :: rest ->
            let substrings = get_substrings text in
            let neighbors = Seq.flat_map (fun s -> StringSet.to_seq @@ generate_neighborhood s d) substrings in
            let motifs = neighbors |> Seq.filter all_texts_approximately_contain_pattern |> StringSet.of_seq in
            loop (StringSet.union acc motifs) rest in
    loop StringSet.empty texts


let () =
    let k, d = Scanf.sscanf (read_line ()) "%d %d" (fun k d -> (k, d)) in
    let dnas = read_dna_strings () in
    let result = motif_enumeration dnas k d in
    StringSet.elements result |> String.concat " " |> print_endline
