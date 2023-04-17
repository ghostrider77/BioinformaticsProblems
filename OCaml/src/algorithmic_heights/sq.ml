module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

type edge = { a : int; b : int }

module Graph : sig
    type t
    val create : int -> edge list -> t
    val get_neighbors : t -> int -> IntSet.t
    val nr_nodes : t -> int
end = struct
    type t = { nr_nodes : int; adjacency_list : IntSet.t IntMap.t }

    let nr_nodes { nr_nodes; _ } = nr_nodes

    let get_neighbors { adjacency_list; _ } node =
        match IntMap.find_opt node adjacency_list with
            | None -> IntSet.empty
            | Some neighbors -> neighbors

    let create nr_nodes edges =
        let add_edge adjacency_list { a; b } =
            let update_function = function
                | None -> Some (IntSet.singleton b)
                | Some ns -> Some (IntSet.add b ns) in
            IntMap.update a update_function adjacency_list in
        let add_edge_both_way adjacency_list { a; b } =
            add_edge (add_edge adjacency_list { a; b }) { a = b; b = a } in
        let adjacency_list = List.fold_left add_edge_both_way IntMap.empty edges in
        { nr_nodes; adjacency_list }
end


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


let has_cycle_of_length_four (graph : Graph.t) : bool =
    let nr_nodes = Graph.nr_nodes graph in
    let nodes = Seq.init nr_nodes (fun k -> k + 1) in
    let predicate node1 =
        let neighbors = Graph.get_neighbors graph node1 in
        if IntSet.is_empty neighbors then false
        else
            let further_nodes = Seq.init (nr_nodes - node1) (fun k -> k + node1 + 1) in
            Seq.exists (fun node2 -> IntSet.(node2 |> Graph.get_neighbors graph |> inter neighbors |> cardinal) >= 2)
            further_nodes in
    Seq.exists predicate nodes


let () =
    let nr_examples = read_int () in
    let graphs = read_graphs nr_examples in
    let results = List.map has_cycle_of_length_four graphs in
    results |> List.map (fun res -> if res then "1" else "-1") |> String.concat " " |> print_endline
