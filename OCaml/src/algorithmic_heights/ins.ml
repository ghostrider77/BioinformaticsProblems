let convert_to_ints (line : string) : int array =
    Array.of_list @@ List.map int_of_string Str.(line |> split (regexp " "))


let calc_nr_swaps_in_insertion_sort (array : int array) (n : int) : int =
    let swap i j =
        let elem = array.(i) in
        array.(i) <- array.(j);
        array.(j) <- elem in
    let rec move_elem_to_its_place nr_swaps k =
        if k = 0 || array.(k) >= array.(k - 1) then nr_swaps
        else
            (swap k (k - 1);
            move_elem_to_its_place (nr_swaps + 1) (k - 1)) in
    Seq.(fold_left move_elem_to_its_place 0 @@ init (n - 1) (fun ix -> ix + 1))


let () =
    let n = read_int () in
    let array = convert_to_ints (read_line ()) in
    let result = calc_nr_swaps_in_insertion_sort array n in
    print_int result; print_newline ()
