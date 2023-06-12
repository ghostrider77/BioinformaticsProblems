let calc_string_spelled_by_a_genome_path = function
    | [] -> ""
    | (x :: _) as k_mers ->
        let k = String.length x in
        k_mers |> List.mapi (fun ix k_mer -> if ix = 0 then k_mer else String.sub k_mer (k - 1) 1) |> String.concat ""


let read_k_mers () =
    let rec loop acc =
        try
            let dna = read_line () in
            if dna = String.empty then List.rev acc
            else loop (dna :: acc)
        with End_of_file -> List.rev acc in
    loop []


let () =
    let k_mers = read_k_mers () in
    let result = calc_string_spelled_by_a_genome_path k_mers in
    print_endline result
