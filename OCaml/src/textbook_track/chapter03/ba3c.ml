module StringMap = Map.Make(String)

module OverlapGraph : sig
    type t
    val create : string list -> t
    val edges : t -> (string * string) Seq.t
end = struct
    type t = { adjacency_list : string list StringMap.t }

    let get_or_else key graph =
        match StringMap.find_opt key graph with
            | None -> []
            | Some value -> value

    let assign_prefix_to_pattern k_mers k =
        let rec loop acc = function
            | [] -> acc
            | k_mer :: rest ->
                let prefix = String.sub k_mer 0 (k - 1) in
                loop (StringMap.add prefix (k_mer :: get_or_else prefix acc) acc) rest in
        loop StringMap.empty k_mers

    let create = function
        | [] -> { adjacency_list = StringMap.empty }
        | (k_mer :: _) as k_mers ->
            let k = String.length k_mer in
            let prefix_to_pattern = assign_prefix_to_pattern k_mers k in
            let process_k_mer graph k_mer =
                let suffix = String.sub k_mer 1 (k - 1) in
                let neighbors = get_or_else suffix prefix_to_pattern in
                StringMap.add k_mer (neighbors @ get_or_else k_mer graph) graph in
            {adjacency_list = List.fold_left process_k_mer StringMap.empty k_mers}

    let edges { adjacency_list } =
        let items = StringMap.to_seq adjacency_list in
        Seq.flat_map (fun (k_mer, neighbors) -> List.(neighbors |> map (fun neighbor -> (k_mer, neighbor)) |> to_seq))
            items
end


let read_k_mers () =
    let rec loop acc =
        try
            let dna = read_line () in
            if dna = String.empty then List.rev acc
            else loop (dna :: acc)
        with End_of_file -> List.rev acc in
    loop []


let () =
    let k_mers = read_k_mers () in
    let graph = OverlapGraph.create k_mers in
    let edges = OverlapGraph.edges graph in
    Seq.iter (fun (node, neighbor) -> print_endline @@ Printf.sprintf "%s -> %s" node neighbor) edges
