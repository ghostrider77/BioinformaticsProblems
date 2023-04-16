open Batteries


let convert_to_intlist (line : string) : int list =
    List.map int_of_string Str.(line |> split (regexp " "))


let merge_sorted_lists (lst1 : int list) (n1 : int) (lst2 : int list) (inversions : int) : int list * int =
    let rec loop acc inv xs length ys = match (xs, ys) with
        | ([], []) -> (List.rev acc, inv)
        | (x :: xss, []) -> loop (x :: acc) inv xss (length - 1) ys
        | ([], y :: yss) -> loop (y :: acc) inv xs length yss
        | (x :: xss, y :: yss) ->
            if x <= y then loop (x :: acc) inv xss (length - 1) ys
            else loop (y :: acc) (inv + length) xs length yss in
    loop [] inversions lst1 n1 lst2


let rec count_inversions (xs : int list) (n : int) : int list * int =
    if n <= 1 then (xs, 0)
    else
        let k = n / 2 in
        let (n1, n2) = (k, n - k) in
        let xs1, xs2 = List.split_at k xs in
        let (sorted1, inv1) = count_inversions xs1 n1 in
        let (sorted2, inv2) = count_inversions xs2 n2 in
        merge_sorted_lists sorted1 n1 sorted2 (inv1 + inv2)


let () =
    let n = read_int () in
    let xs = convert_to_intlist (read_line ()) in
    let (_, result) = count_inversions xs n in
    print_int result; print_newline ()
