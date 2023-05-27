type profile_column = { a : float; c : float; g : float; t : float }


let get_profile_value {a; c; g; t} = function
    | 'A' -> a
    | 'C' -> c
    | 'G' -> g
    | 'T' -> t
    | _ -> failwith "Unknown nucleotide."


let convert_to_float_list (line : string) : float list =
    List.map float_of_string Str.(line |> split (regexp " "))


let read_profile_matrix () : profile_column list =
    let rows = List.init 4 (fun _ -> convert_to_float_list @@ read_line ()) in
    let profile_of_list = function
        | [a; c; g; t] -> { a; c; g; t }
        | _ -> raise (Invalid_argument "4 values are expected in each column.") in
    let open Batteries in
    List.(rows |> transpose |> map profile_of_list)


let calc_k_mer_probability (k_mer : string) (profile_matrix : profile_column list) : float =
    Seq.fold_left2 (fun acc nucleotide column -> acc *. (get_profile_value column nucleotide)) 1.0
        (String.to_seq k_mer) (List.to_seq profile_matrix)


let profile_most_probable_k_mer (text : string) (profile_matrix : profile_column list) (k : int) : string =
    let n = String.length text in
    let k_mers = Seq.init (n - k + 1) (fun ix -> String.sub text ix k) in
    let process_k_mer ((max_probability, _) as acc) k_mer =
        let p = calc_k_mer_probability k_mer profile_matrix in
        if p > max_probability then (p, k_mer) else acc in
    snd @@ Seq.fold_left process_k_mer (0.0, "") k_mers


let () =
    let text = read_line () in
    let k = read_int () in
    let matrix_columns = read_profile_matrix () in
    let result = profile_most_probable_k_mer text matrix_columns k in
    print_endline result
