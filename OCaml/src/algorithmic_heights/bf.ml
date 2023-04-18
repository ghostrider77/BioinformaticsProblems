module IntMap = Map.Make(Int)

type edge = { a : int; b : int ; weight : int }


module Graph : sig
    type t
    val adjacency_list : t -> edge list IntMap.t
    val create : int -> edge list -> t
    val get_neighbors : t -> int -> edge list
end = struct
    type t = { nr_nodes : int; adjacency_list : edge list IntMap.t }

    let adjacency_list { adjacency_list; _ } = adjacency_list

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


let bellman_ford (graph : Graph.t) (nr_nodes : int) (start_node : int) : float list =
    let distances = Array.make nr_nodes infinity in
    distances.(start_node - 1) <- 0.0;
    let update_distances node neighbors =
        let dist = distances.(node - 1) in
        if dist < infinity then
            let update {b = neighbor; weight; _ } =
                let dist_through_node = dist +. float weight in
                if distances.(neighbor - 1) > dist_through_node then distances.(neighbor - 1) <- dist_through_node in
            List.iter update neighbors in
    for _ = 1 to nr_nodes do
        IntMap.iter update_distances (Graph.adjacency_list graph)
    done;
    Array.to_list distances


let () =
    let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
    let edges = read_edges nr_edges in
    let graph = Graph.create nr_nodes edges in
    let result = bellman_ford graph nr_nodes 1 in
    result |> List.map (fun d -> if d = infinity then "x" else string_of_int (int_of_float d))
           |> String.concat " "
           |> print_endline
