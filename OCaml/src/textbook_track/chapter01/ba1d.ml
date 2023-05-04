let find_pattern_occurrences (text : string) (pattern : string) : int list =
    let text_length = String.length text in
    let pattern_length = String.length pattern in
    let rec loop acc ix =
        if ix = text_length - pattern_length + 1 then List.rev acc
        else
            let acc' = if (String.sub text ix pattern_length) = pattern then ix :: acc else acc in
            loop acc' (ix + 1) in
    loop [] 0


let () =
    let pattern = read_line () in
    let text = read_line () in
    let result = find_pattern_occurrences text pattern in
    result |> List.map string_of_int |> String.concat " " |> print_endline
