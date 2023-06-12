let calc_k_mer_composition (text : string) (k : int) : string list =
    let n = String.length text in
    let k_mers = List.init (n - k + 1) (fun ix -> String.sub text ix k) in
    List.sort compare k_mers


let () =
    let k = read_int () in
    let text = read_line () in
    let result = calc_k_mer_composition text k in
    List.iter print_endline result
