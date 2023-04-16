let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let three_way_partitioning (arr : int array) (pivot : int) (n : int) : unit =
    let swap i j =
        let elem = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- elem in
    let rec loop middle_ix current_ix end_ix =
        if current_ix > end_ix then ()
        else if arr.(current_ix) < pivot then
            (swap current_ix middle_ix;
            loop (middle_ix + 1) (current_ix + 1) end_ix)
        else if arr.(current_ix) > pivot then
            (swap current_ix end_ix;
            loop middle_ix current_ix (end_ix - 1))
        else
            loop middle_ix (current_ix + 1) end_ix in
    loop 0 1 (n - 1)


let () =
    let n = read_int () in
    let arr = convert_to_ints (read_line ()) in
    three_way_partitioning arr arr.(0) n;
    Array.(arr |> map string_of_int |> to_list) |> String.concat " " |> print_endline
