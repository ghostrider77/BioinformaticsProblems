open Batteries


let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let merge_sorted_lists (lst1 : int list) (lst2 : int list) : int list =
    let rec loop acc xs ys = match (xs, ys) with
        | ([], []) -> List.rev acc
        | (x :: xss, []) -> loop (x :: acc) xss ys
        | ([], y :: yss) -> loop (y :: acc) xs yss
        | (x :: xss, y :: yss) -> if x <= y then loop (x :: acc) xss ys else loop (y :: acc) xs yss
    in loop [] lst1 lst2


let rec merge_sort (xs : int list) (n : int) : int list =
    if n <= 1 then xs
    else
        let k = n / 2 in
        let xs1, xs2 = List.split_at k xs in
        let sorted1 = merge_sort xs1 k in
        let sorted2 = merge_sort xs2 (n - k) in
        merge_sorted_lists sorted1 sorted2


let () =
    let n = read_int () in
    let xs = convert_to_intlist (read_line ()) in
    let result = merge_sort xs n in
    result |> List.map string_of_int |> String.concat " " |> print_endline
