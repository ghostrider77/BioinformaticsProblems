module IntMap = Map.Make(Int)

type result = NotFound | IndexPair of int * int


let string_of_result = function
    | NotFound -> "-1"
    | IndexPair (i, j) -> Printf.sprintf "%d %d" i j


let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let read_arrays (k : int) : int list list =
    List.(map (fun _ -> convert_to_intlist (read_line ())) @@ init k (fun ix -> ix))


let find_zero_sum_index_pairs (arrays : int list list) : result list =
    let solve_2sum arr =
        let rec loop negative_target_indices ix = function
            | [] -> NotFound
            | x :: xss ->
                match IntMap.find_opt x negative_target_indices with
                    | None -> loop (IntMap.add (-x) ix negative_target_indices) (ix + 1) xss
                    | Some jy -> IndexPair (jy + 1, ix + 1) in
        loop IntMap.empty 0 arr in
    List.map solve_2sum arrays


let () =
    let k = Scanf.sscanf (read_line ()) "%d %d" (fun k _ -> k) in
    let arrays = read_arrays k in
    let results = find_zero_sum_index_pairs arrays in
    List.iter (fun res -> print_endline @@ string_of_result res) results
