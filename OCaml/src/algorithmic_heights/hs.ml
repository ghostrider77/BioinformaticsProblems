let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let get_index_of_parent_children_maximum (arr : int array) (parent_ix : int) (size : int) : int =
    let left_child_ix = 2 * parent_ix + 1 in
    let right_child_ix = left_child_ix + 1 in
    let max_ix = if left_child_ix < size && arr.(left_child_ix) > arr.(parent_ix) then left_child_ix else parent_ix in
    if right_child_ix < size && arr.(right_child_ix) > arr.(max_ix) then right_child_ix
    else max_ix


let swap arr i j =
    let elem = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- elem


let sift_down (arr : int array) (parent_ix : int) (n : int) : unit =
    let rec loop current_parent_ix =
        let max_ix = get_index_of_parent_children_maximum arr current_parent_ix n in
        if max_ix <> current_parent_ix then
            (swap arr max_ix current_parent_ix;
            loop max_ix) in
    loop parent_ix


let heapify (arr : int array) (n : int) : unit =
    for parent_ix = (n / 2 - 1) downto 0 do
        sift_down arr parent_ix n;
    done


let heapsort (arr : int array) (n : int) : unit =
    heapify arr n;
    for k = n downto 2 do
        swap arr 0 (k - 1);
        sift_down arr 0 (k - 1)
    done


let () =
    let n = read_int () in
    let arr = convert_to_ints (read_line ()) in
    heapsort arr n;
    Array.(arr |> map string_of_int |> to_list) |> String.concat " " |> print_endline
