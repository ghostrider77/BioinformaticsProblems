module StringSet = Set.Make(String)

let nucleotides = ['A'; 'C'; 'G'; 'T']


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


let () =
    let dna = read_line () in
    let d = read_int () in
    let result = generate_neighborhood dna d in
    StringSet.iter print_endline result
