let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let three_way_partitioning (arr : int array) (pivot : int) (start_ix : int) (end_ix : int) : int * int =
    let swap i j =
        let elem = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- elem in
    let rec loop current_ix start_ix end_ix =
        if current_ix > end_ix then (start_ix, end_ix + 1)
        else if arr.(current_ix) < pivot then
            (swap current_ix start_ix;
            loop (current_ix + 1) (start_ix + 1) end_ix)
        else if arr.(current_ix) > pivot then
            (swap current_ix end_ix;
            loop current_ix start_ix (end_ix - 1))
        else
            loop (current_ix + 1) start_ix end_ix in
    loop start_ix start_ix end_ix


let quicksort (arr : int array) (n : int) : unit =
    Random.init 2112;
    let rec loop = function
        | [] -> ()
        | (start_ix, end_ix) :: rest ->
            if start_ix >= end_ix then loop rest
            else
                let random_ix = start_ix + Random.int (end_ix - start_ix + 1) in
                let pivot = arr.(random_ix) in
                let middle_start, middle_end = three_way_partitioning arr pivot start_ix end_ix in
                loop ((start_ix, middle_start - 1) :: (middle_end, end_ix) :: rest) in
    loop [(0, n - 1)]


let () =
    let n = read_int () in
    let arr = convert_to_ints (read_line ()) in
    quicksort arr n;
    Array.(arr |> map string_of_int |> to_list) |> String.concat " " |> print_endline
