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


let create_profile_matrix (motifs : string array) (k : int) : profile_column array =
    let nr_motifs = float @@ Array.length motifs in
    let pcnt = 1.0 /. (nr_motifs +. 4.0) in
    let update_nucleotide_count ({a; c; g; t} as counts) = function
        | 'A' -> { counts with a = a +. pcnt }
        | 'C' -> { counts with c = c +. pcnt }
        | 'G' -> { counts with g = g +. pcnt }
        | 'T' -> { counts with t = t +. pcnt }
        | _ -> failwith "Unknown nucleotide." in
    let create_profile_column (chars : char array) : profile_column =
        Array.fold_left update_nucleotide_count {a = pcnt; c = pcnt; g = pcnt; t = pcnt} chars in
    Array.map (fun ix -> create_profile_column @@ Array.map (fun s -> s.[ix]) motifs) @@ Array.init k Fun.id


let calc_profile_matrix_score (motifs : string array) (k : int) : int =
    let profile_matrix = create_profile_matrix motifs k in
    let consensus =
        profile_matrix
            |> Array.to_list
            |> List.map (fun c -> c |> most_probable_nucleotide |> String.make 1)
            |> String.concat "" in
    Array.fold_left (fun score motif -> score + calc_hamming_distance motif consensus) 0 motifs


let calc_k_mer_probability (k_mer : string) (profile_matrix : profile_column array) : float =
    Seq.fold_left2 (fun acc nucleotide column -> acc *. (get_nucleotide_probability column nucleotide)) 1.0
        (String.to_seq k_mer) (Array.to_seq profile_matrix)


let get_profile_randomly_generated_k_mer (text : string) (profile_matrix : profile_column array) (k : int) : string =
    let k_mers = Seq.init (String.length text - k + 1) (fun ix -> String.sub text ix k) in
    let weights = Seq.map (fun k_mer -> calc_k_mer_probability k_mer profile_matrix) k_mers in
    let total_weight = Seq.fold_left (+.) 0.0 weights in
    let cumulative_sums = Seq.(weights |> scan (+.) 0.0 |> drop 1 |> mapi (fun ix s -> (ix, s))) in
    let r = Random.float 1.0 in
    match Seq.find (fun (_, s) -> r *. total_weight <= s) cumulative_sums with
        | None -> failwith "Could not determine random substring."
        | Some (index, _) -> String.sub text index k


let select_random_motifs (texts : string array) (k : int) : string array =
    let select_random_substring text =
        let ix = Random.int (String.length text - k + 1) in
        String.sub text ix k in
    Array.map select_random_substring texts


let remove_selected_row (motifs : string array) (ix : int) : string array =
    motifs |> Array.to_seqi
           |> Seq.filter_map (fun (i, motif) -> if i = ix then None else Some motif)
           |> Array.of_seq


let gibbs_sampling (texts : string array) (k : int) (t : int) (n : int) : string array * int =
    let initial_motifs = select_random_motifs texts k in
    let initial_score = calc_profile_matrix_score initial_motifs k in
    let select_better_motifs (current_best_motif, current_best_score, motifs) _ =
        let ix = Random.int t in
        let reduced_motifs = remove_selected_row motifs ix in
        let profile = create_profile_matrix reduced_motifs k in
        let motif = get_profile_randomly_generated_k_mer texts.(ix) profile k in
        let updated_motifs = Array.init t (fun i -> if i = ix then motif else motifs.(i)) in
        let score = calc_profile_matrix_score updated_motifs k in
        if score < current_best_score then (updated_motifs, score, updated_motifs)
        else (current_best_motif, current_best_score, updated_motifs) in
    let best_motif, best_score, _ =
        Seq.fold_left select_better_motifs ((initial_motifs, initial_score, initial_motifs)) (Seq.init n Fun.id) in
    (best_motif, best_score)


let run_gibbs_sampling (texts : string array) (k : int) (t : int) (n : int) (nr_iterations : int) : string array =
    let run_sampling ((_, best_score) as acc) _ =
        let motifs, score = gibbs_sampling texts k t n in
        if score < best_score then (motifs, score) else acc in
    fst @@ Seq.fold_left run_sampling ([||], k * Array.length texts) (Seq.init nr_iterations Fun.id)


let () =
    let k, t, n = Scanf.sscanf (read_line ()) "%d %d %d" (fun k t n -> (k, t, n)) in
    let texts = Array.init t (fun _ -> read_line()) in
    Random.init 2112;
    let result = run_gibbs_sampling texts k t n 20 in
    Array.iter print_endline result
