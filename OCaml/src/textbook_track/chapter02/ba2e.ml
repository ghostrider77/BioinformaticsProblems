type profile_column = { a : float; c : float; g : float; t : float }

let nucleotides = ['A'; 'C'; 'G'; 'T']


let get_nucleotide_probability {a; c; g; t} = function
    | 'A' -> a
    | 'C' -> c
    | 'G' -> g
    | 'T' -> t
    | _ -> failwith "Unknown nucleotide."


let most_probable_nucleotide {a; c; g; t} =
    let nucleotide, _ =
        List.fold_left2 (fun (n, max_p) p nucleotide -> if p >= max_p then (nucleotide, p) else (n, max_p) ) (' ', 0.0)
        [a; c; g; t] nucleotides in
    nucleotide


let calc_hamming_distance (s1 : string) (s2 : string) : int =
    Seq.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 (String.to_seq s1) (String.to_seq s2)


let create_profile_matrix (motifs : string list) (k : int) : profile_column list =
    let nr_motifs = float @@ List.length motifs in
    let pcnt = 1.0 /. (nr_motifs +. 4.0) in
    let update_nucleotide_count ({a; c; g; t} as counts) = function
        | 'A' -> { counts with a = a +. pcnt }
        | 'C' -> { counts with c = c +. pcnt }
        | 'G' -> { counts with g = g +. pcnt }
        | 'T' -> { counts with t = t +. pcnt }
        | _ -> failwith "Unknown nucleotide." in
    let create_profile_column (chars : char list) : profile_column =
        List.fold_left update_nucleotide_count {a = pcnt; c = pcnt; g = pcnt; t = pcnt} chars in
    List.map (fun ix -> create_profile_column @@ List.map (fun s -> s.[ix]) motifs) @@ List.init k Fun.id


let calc_profile_matrix_score (motifs : string list) (k : int) : int =
    let profile_matrix = create_profile_matrix motifs k in
    let consensus =
        profile_matrix |> List.map (fun c -> c |> most_probable_nucleotide |> String.make 1) |> String.concat "" in
    List.fold_left (fun score motif -> score + calc_hamming_distance motif consensus) 0 motifs


let calc_k_mer_probability (k_mer : string) (profile_matrix : profile_column list) : float =
    Seq.fold_left2 (fun acc nucleotide column -> acc *. (get_nucleotide_probability column nucleotide)) 1.0
        (String.to_seq k_mer) (List.to_seq profile_matrix)


let profile_most_probable_k_mer (text : string) (profile_matrix : profile_column list) (k : int) : string =
    let n = String.length text in
    let k_mers = Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
    let process_k_mer ((max_probability, _) as acc) k_mer =
        let p = calc_k_mer_probability k_mer profile_matrix in
        if p > max_probability then (p, k_mer) else acc in
    snd @@ Seq.fold_left process_k_mer (0.0, String.sub text 0 k) k_mers


let select_motif_from_each_text (initial_motif : string) (texts : string list) (k : int) : string list =
    let rec loop motifs = function
        | [] -> List.rev motifs
        | text :: rest ->
            let profile = create_profile_matrix motifs k in
            let motif = profile_most_probable_k_mer text profile k in
            loop (motif :: motifs) rest in
    loop [initial_motif] texts


let improved_greedy_motif_search (texts : string list) (k : int) : (string list) =
    match texts with
        | [] -> []
        | text :: rest ->
            let initial_motifs = List.map (fun s -> String.sub s 0 k) texts in
            let initial_score = calc_profile_matrix_score initial_motifs k in
            let update_motifs ((_, best_score) as acc) k_mer =
                let motifs = select_motif_from_each_text k_mer rest k in
                let score = calc_profile_matrix_score motifs k in
                if score < best_score then (motifs, score) else acc in
            let n = String.length text in
            let k_mers = Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
            fst @@ Seq.fold_left update_motifs (initial_motifs, initial_score) k_mers


let () =
    let k, t = Scanf.sscanf (read_line ()) "%d %d" (fun k t -> (k, t)) in
    let texts = List.init t (fun _ -> read_line()) in
    let result = improved_greedy_motif_search texts k in
    List.iter print_endline result
