let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let two_way_partitioning (arr : int array) (pivot : int) (n : int) : unit =
    let swap i j =
        let elem = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- elem in
    let rec loop start_ix end_ix =
        if start_ix > end_ix then ()
        else if arr.(start_ix) <= pivot then
            (swap start_ix (start_ix - 1);
            loop (start_ix + 1) end_ix)
        else
            (swap start_ix end_ix;
            loop start_ix (end_ix - 1)) in
    loop 1 (n - 1)


let () =
    let n = read_int () in
    let arr = convert_to_ints (read_line ()) in
    two_way_partitioning arr arr.(0) n;
    Array.(arr |> map string_of_int |> to_list) |> String.concat " " |> print_endline
