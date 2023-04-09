let calc_fibonacci (n : int) : int =
    let rec loop a b k =
        if k > n then a
        else loop b (a + b) (k + 1) in
    loop 0 1 1


let () =
    let n = read_int () in
    let result = calc_fibonacci n in
    print_int result; print_newline ()
