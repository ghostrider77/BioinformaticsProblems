module IntMap = Hashtbl.Make(
    struct
        type t = int
        let equal = Stdlib.(=)
        let hash i = i land max_int
    end
)

type result = NotFound | IndexTriple of int * int * int


let string_of_result = function
    | NotFound -> "-1"
    | IndexTriple (i, j, k) -> Printf.sprintf "%d %d %d" i j k


let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let read_arrays (k : int) : int list list =
    List.(map (fun _ -> convert_to_intlist (read_line ())) @@ init k (fun ix -> ix))


let solve_2sum (xs : int list) (target : int) (target_ix : int) : result =
    let negative_target_indices = IntMap.create (List.length xs) in
    let rec loop = function
        | [] -> NotFound
        | (ix, item) :: rest ->
            match IntMap.find_opt negative_target_indices item with
                | None ->
                    IntMap.add negative_target_indices (target - item) ix;
                    loop rest
                | Some jy -> IndexTriple (target_ix + 1, jy + 1, ix + 1) in
    let xss = xs |> List.to_seq |> Seq.mapi (fun i x -> (i, x)) |> Seq.drop (target_ix + 1) |> List.of_seq in
    loop xss

let find_zero_sum_index_pairs (arrays : int list list) : result list =
    let solve_3sum xs =
        let first_valid_result =
            xs |> List.to_seq
               |> Seq.mapi (fun ix item -> solve_2sum xs (-item) ix)
               |> Seq.find (fun res -> res <> NotFound) in
        match first_valid_result with
            | None -> NotFound
            | Some indices -> indices in
    List.map solve_3sum arrays


let () =
    let k = Scanf.sscanf (read_line ()) "%d %d" (fun k _ -> k) in
    let arrays = read_arrays k in
    let results = find_zero_sum_index_pairs arrays in
    List.iter (fun res -> print_endline @@ string_of_result res) results
