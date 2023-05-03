let count_pattern (text : string) (pattern : string) : int =
    let text_length = String.length text in
    let pattern_length = String.length pattern in
    let rec loop acc ix =
        if ix = text_length - pattern_length + 1 then acc
        else
            let acc' = if (String.sub text ix pattern_length) = pattern then acc + 1 else acc in
            loop acc' (ix + 1) in
    loop 0 0


let () =
    let text = read_line () in
    let pattern = read_line () in
    let result = count_pattern text pattern in
    print_int result; print_newline ()
