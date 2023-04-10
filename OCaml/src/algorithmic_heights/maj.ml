let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let read_arrays (k : int) : int list list =
    List.(map (fun _ -> convert_to_intlist (read_line ())) @@ init k (fun ix -> ix))


let calc_majority_elements (arrays : int list list) (n : int) : int list =
    let majority_elem xs =
        let counter = Hashtbl.create n in
        let get_or_else table key default =
            match Hashtbl.find_opt table key with
                | None -> default
                | Some x -> x in
        let update_counter x =
            let count = get_or_else counter x 0 in
            Hashtbl.replace counter x (count + 1) in
        List.iter update_counter xs;
        let find_max item cnt (most_common, max_cnt) =
            if cnt > max_cnt then (item, cnt)
            else (most_common, max_cnt) in
        let (most_common, count) = Hashtbl.fold find_max counter (0, -1) in
        if count > n / 2 then most_common
        else -1 in
    List.map majority_elem arrays


let () =
    let (k, n) = Scanf.sscanf (read_line ()) "%d %d" (fun k n -> (k, n)) in
    let arrays = read_arrays k in
    let result = calc_majority_elements arrays n in
    result |> List.map string_of_int |> String.concat " " |> print_endline
