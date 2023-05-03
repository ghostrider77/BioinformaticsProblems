let most_frequent_k_mers (text : string) (k : int) : string list =
    let n = String.length text in
    let counter = Hashtbl.create (n - k) in
    let get_or_else table key default =
        match Hashtbl.find_opt table key with
            | None -> default
            | Some x -> x in
    let update_counter k_mer =
        let count = get_or_else counter k_mer 0 in
        Hashtbl.replace counter k_mer (count + 1) in
    let indices = Seq.init (n - k) (fun ix -> ix) in
    Seq.iter (fun ix -> update_counter (String.sub text ix k)) indices;
    let max_count = Hashtbl.fold (fun _ count acc -> max count acc) counter 0 in
    Hashtbl.fold (fun k_mer count acc -> if count = max_count then k_mer :: acc else acc) counter []


let () =
    let text = read_line () in
    let k = read_int () in
    let result = most_frequent_k_mers text k in
    result |> String.concat " " |> print_endline
