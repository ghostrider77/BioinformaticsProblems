type edge = { a : int; b : int; weight : int }

type graph = { nr_nodes : int; edges : edge list }


let read_edges (nr_edges : int) : edge list =
    let parse_line line =
        Scanf.sscanf line "%d %d %d" (fun a b weight -> { a ; b ; weight }) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range


let read_graphs (n : int) : graph list =
    let read_graph () =
        let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
        let edges = read_edges nr_edges in
        { nr_nodes; edges } in
    List.(map (fun _ -> read_graph ()) @@ init n (fun _ -> 0))


let update_distances (edges : edge list) (distances : float array) : bool =
    let rec loop is_updated = function
        | [] -> is_updated
        | { a = node; b = neighbor; weight } :: rest ->
            let distance_through_node = distances.(node - 1) +. float weight in
            if distances.(neighbor - 1) > distance_through_node then
                (distances.(neighbor - 1) <- distance_through_node;
                loop true rest)
            else loop is_updated rest in
    loop false edges


let has_negative_cycle ({nr_nodes; edges} : graph) : bool =
    let distances = Array.make nr_nodes 0.0 in
    let is_updated = ref false in
    for _ = 1 to nr_nodes do
        is_updated := update_distances edges distances
    done;
    !is_updated


let () =
    let nr_examples = read_int () in
    let graphs = read_graphs nr_examples in
    let result = List.map has_negative_cycle graphs in
    result |> List.map (fun res -> if res then "1" else "-1") |> String.concat " " |> print_endline
