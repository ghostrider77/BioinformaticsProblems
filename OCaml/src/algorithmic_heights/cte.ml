module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

type edge = { a : int; b : int ; weight : int }


module Graph : sig
    type t
    val create : int -> edge list -> t
    val get_neighbors : t -> int -> edge list
    val nr_nodes : t -> int
end = struct
    type t = { nr_nodes : int; adjacency_list : edge list IntMap.t }

    let nr_nodes { nr_nodes; _ } = nr_nodes

    let get_neighbors { adjacency_list; _ } node =
        match IntMap.find_opt node adjacency_list with
            | None -> []
            | Some neighbors -> neighbors

    let create nr_nodes edges =
        let add_edge adjacency_list ({ a; _} as edge) =
            let update_function = function
                | None -> Some [edge]
                | Some ns -> Some (edge :: ns) in
            IntMap.update a update_function adjacency_list in
        let adjacency_list = List.fold_left add_edge IntMap.empty edges in
        { nr_nodes; adjacency_list }
end


let read_edges (nr_edges : int) : edge list =
    let parse_line line =
        Scanf.sscanf line "%d %d %d" (fun a b weight -> { a ; b ; weight }) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range


let read_graphs (n : int) : (Graph.t * edge) list =
    let read_graph () =
        ignore (read_line ());
        let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
        let edges = read_edges nr_edges in
        (Graph.create nr_nodes edges, List.hd edges) in
    List.(map (fun _ -> read_graph ()) @@ init n (fun _ -> 0))


let calc_shortest_distances (graph : Graph.t) (nr_nodes : int) (source_node : int) : int array =
    let distances = Array.make nr_nodes infinity in
    distances.(source_node - 1) <- 0.0;
    let find_smallest node ((_, current_min_dist) as acc) =
        let dist = distances.(node - 1) in
        if dist <= current_min_dist then (node, dist) else acc in
    let update_distance distance_to_node { b; weight; _ } =
        let distance_through_node = distance_to_node +. float weight in
        if distances.(b - 1) > distance_through_node then distances.(b - 1) <- distance_through_node in
    let rec loop nodes =
        if not (IntSet.is_empty nodes) then
            let (v, dist_v) = IntSet.fold find_smallest nodes (0, infinity) in
            let neighbors = Graph.get_neighbors graph v in
            List.iter (update_distance dist_v) neighbors;
            loop (IntSet.remove v nodes) in
    loop (IntSet.of_seq (Seq.init nr_nodes (fun k -> k + 1)));
    Array.map (fun d -> if d = infinity then -1 else int_of_float d) distances


let calc_shortest_cycle_through_given_edge ((graph, { a; b; weight }) : Graph.t * edge) : int =
    let distances = calc_shortest_distances graph (Graph.nr_nodes graph) b in
    let dist = distances.(a - 1) in
    if dist >= 0 then dist + weight else dist


let () =
    let nr_examples = read_int () in
    let graphs = read_graphs nr_examples in
    let results = List.map calc_shortest_cycle_through_given_edge graphs in
    results |> List.map string_of_int |> String.concat " " |> print_endline
