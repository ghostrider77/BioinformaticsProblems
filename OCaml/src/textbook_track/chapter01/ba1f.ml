open Batteries

type skewness = { skew : int; min_skew : int; positions : int list }


let calc_smallest_skew_positions (genome : string) : int list =
    let process ({ skew; min_skew; positions } as acc) ix nucleotide =
        let skew' = if nucleotide = 'C' then skew - 1 else if nucleotide = 'G' then skew + 1 else skew in
        if skew' = min_skew then { acc with skew = skew'; positions = (ix + 1) :: positions }
        else if skew' < min_skew then { skew = skew'; min_skew = skew'; positions = [ix + 1] }
        else { acc with skew = skew' } in
    let { positions; _ } = String.fold_lefti process { skew = 0; min_skew = 0; positions = [0] } genome in
    List.rev positions


let () =
    let genome = read_line () in
    let result = calc_smallest_skew_positions genome in
    result |> List.map string_of_int |> String.concat " " |> print_endline
