let convert_to_ints line =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let find_elems_in_sorted_array (array : int array) (queries : int array) (n : int) : int array =
    let rec binary_search item left right =
        if left > right then -1
        else
            let middle_ix = (left + right) / 2 in
            let middle_elem = array.(middle_ix) in
            if middle_elem = item then middle_ix + 1
            else if middle_elem < item then binary_search item (middle_ix + 1) right
            else binary_search item left (middle_ix - 1) in
    Array.map (fun x -> binary_search x 0 (n - 1)) queries


let () =
    let n = read_int () in
    let _ = read_int () in
    let array = convert_to_ints (read_line ()) in
    let queries = convert_to_ints (read_line ()) in
    let results = find_elems_in_sorted_array array queries n in
    Array.(results |> map string_of_int |> to_list) |> String.concat " " |> print_endline
