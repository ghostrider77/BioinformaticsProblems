let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let merge_sorted_lists (lst1 : int list) (lst2 : int list) : int list =
    let rec loop acc xs ys = match (xs, ys) with
        | ([], []) -> List.rev acc
        | (x :: xss, []) -> loop (x :: acc) xss ys
        | ([], y :: yss) -> loop (y :: acc) xs yss
        | (x :: xss, y :: yss) -> if x <= y then loop (x :: acc) xss ys else loop (y :: acc) xs yss
    in loop [] lst1 lst2


let () =
    ignore (read_int ());
    let xs = convert_to_intlist (read_line ()) in
    ignore (read_int ());
    let ys = convert_to_intlist (read_line ()) in
    let result = merge_sorted_lists xs ys in
    result |> List.map string_of_int |> String.concat " " |> print_endline
