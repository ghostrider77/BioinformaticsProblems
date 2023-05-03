let nucleotides = ['A'; 'C'; 'G'; 'T']


let number_to_pattern (encoding : int) (k : int) : string =
    let rec loop pattern number l =
        if l = k then pattern |> List.to_seq |> String.of_seq
        else
            let number' = number / 4 in
            let remainder = number mod 4 in
            loop ((List.nth nucleotides remainder) :: pattern) number' (l + 1) in
    loop [] encoding 0


let () =
    let encoding = read_int () in
    let k = read_int () in
    let result = number_to_pattern encoding k in
    print_endline result
