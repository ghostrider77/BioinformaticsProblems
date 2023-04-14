module IntMap = Map.Make(Int)

type node_color = Red | Blue

type edge = { a : int; b : int }

module Graph : sig
    type t
    val create : int -> edge list -> t
    val get_neighbors : t -> int -> int list
    val nr_nodes : t -> int
end = struct
    type t = { nr_nodes : int; adjacency_list : int list IntMap.t }

    let nr_nodes { nr_nodes; _ } = nr_nodes

    let get_neighbors { adjacency_list; _ } node =
        match IntMap.find_opt node adjacency_list with
            | None -> []
            | Some neighbors -> neighbors

    let create nr_nodes edges =
        let add_edge adjacency_list { a; b } =
            let update_function = function
                | None -> Some [b]
                | Some ns -> Some (b :: ns) in
            IntMap.update a update_function adjacency_list in
        let add_edge_both_way adjacency_list { a; b } =
            add_edge (add_edge adjacency_list { a; b }) { a = b; b = a } in
        let adjacency_list = List.fold_left add_edge_both_way IntMap.empty edges in
        { nr_nodes; adjacency_list }

end


let get_opposite_color = function
    | Red -> Blue
    | Blue -> Red


let read_graphs (n : int) : Graph.t list =
    let read_edges (nr_edges : int) : edge list =
        let parse_line line =
            Scanf.sscanf line "%d %d" (fun a b -> { a; b }) in
        let range = Seq.init nr_edges (fun k -> k) in
        List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range in
    let read_graph () =
        ignore (read_line ());
        let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
        let edges = read_edges nr_edges in
        Graph.create nr_nodes edges in
    List.(map (fun _ -> read_graph ()) @@ init n (fun _ -> 0))


let find_consistent_component_coloring (graph : Graph.t) (start_node : int) : (node_color IntMap.t) option =
    let queue = Queue.create () in
    Queue.add start_node queue;
    let rec loop node_colors_in_component = match Queue.take_opt queue with
        | None -> Some node_colors_in_component
        | Some node ->
            let color = IntMap.find node node_colors_in_component in
            let opposite_color = get_opposite_color color in
            let neighbors = Graph.get_neighbors graph node in
            let (colored_neighbors, uncolored_neighbors) =
                List.partition (fun n -> IntMap.mem n node_colors_in_component) neighbors in
            if List.exists (fun n -> color = IntMap.find n node_colors_in_component) colored_neighbors then None
            else
                (Queue.add_seq queue (List.to_seq uncolored_neighbors);
                let new_nodes = List.(uncolored_neighbors |> map (fun n -> (n, opposite_color)) |> to_seq) in
                loop (IntMap.add_seq new_nodes node_colors_in_component)) in
    loop (IntMap.singleton start_node Red)


let is_bipartite (graph : Graph.t) : bool =
    let rec loop node_colors start_node =
        if start_node >= (Graph.nr_nodes graph) then true
        else if IntMap.mem start_node node_colors then loop node_colors (start_node + 1)
        else
            match find_consistent_component_coloring graph start_node with
                | None -> false
                | Some component_coloring ->
                    let bindings = IntMap.bindings component_coloring in
                    loop (IntMap.add_seq (List.to_seq bindings) node_colors) (start_node + 1) in
    loop IntMap.empty 1


let () =
    let nr_examples = read_int () in
    let graphs = read_graphs nr_examples in
    let results = List.map is_bipartite graphs in
    results |> List.map (fun res -> if res then "1" else "-1") |> String.concat " " |> print_endline
