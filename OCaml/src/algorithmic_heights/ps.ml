let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let get_index_of_parent_children_minimum (arr : int array) (parent_ix : int) (size : int) : int =
    let left_child_ix = 2 * parent_ix + 1 in
    let right_child_ix = left_child_ix + 1 in
    let min_ix = if left_child_ix < size && arr.(left_child_ix) < arr.(parent_ix) then left_child_ix else parent_ix in
    if right_child_ix < size && arr.(right_child_ix) < arr.(min_ix) then right_child_ix
    else min_ix


let swap arr i j =
    let elem = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- elem


let sift_down (arr : int array) (parent_ix : int) (n : int) : unit =
    let rec loop current_parent_ix =
        let min_ix = get_index_of_parent_children_minimum arr current_parent_ix n in
        if min_ix <> current_parent_ix then
            (swap arr min_ix current_parent_ix;
            loop min_ix) in
    loop parent_ix


let heapify (arr : int array) (n : int) : unit =
    for parent_ix = (n / 2 - 1) downto 0 do
        sift_down arr parent_ix n;
    done


let partialsort (arr : int array) (size : int) (k : int) : int list =
    heapify arr size;
    for n = size downto (size - k + 1) do
        swap arr 0 (n - 1);
        sift_down arr 0 (n - 1)
    done;
    List.rev @@ Array.to_list @@ Array.sub arr (size - k) k


let () =
    let n = read_int () in
    let arr = convert_to_ints (read_line ()) in
    let k = read_int () in
    let result = partialsort arr n k in
    result |> List.map string_of_int |> String.concat " " |> print_endline
