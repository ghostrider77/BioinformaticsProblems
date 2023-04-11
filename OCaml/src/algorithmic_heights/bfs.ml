module IntMap = Map.Make(Int)

type edge = { a : int; b : int }

module Graph : sig
    type t
    val create : int -> edge list -> t
    val bfs : t -> int -> int list
end = struct
    type t = { nr_nodes : int; adjacency_list : int list IntMap.t }

    let get_neighbors adjacency_list node =
        match IntMap.find_opt node adjacency_list with
            | None -> []
            | Some neighbors -> neighbors

    let create nr_nodes edges =
        let add_edge adjacency_list { a; b } =
            let update_function = function
                | None -> Some [b]
                | Some ns -> Some (b :: ns) in
            IntMap.update a update_function adjacency_list in
        let adjacency_list = List.fold_left add_edge IntMap.empty edges in
        { nr_nodes; adjacency_list }

    let bfs { nr_nodes; adjacency_list } start_node =
        let distances = Array.make nr_nodes (-1) in
        distances.(start_node - 1) <- 0;
        let queue = Queue.create () in
        Queue.add start_node queue;
        while not (Queue.is_empty queue) do
            let node = Queue.take queue in
            let neighbors = get_neighbors adjacency_list node in
            let update_distance neighbour =
                if distances.(neighbour - 1) = -1 then
                    Queue.add neighbour queue;
                    distances.(neighbour - 1) <- distances.(node - 1) + 1 in
            List.iter update_distance neighbors
        done;
        Array.to_list distances
end


let read_edges (nr_edges : int) : edge list =
    let parse_line line =
        Scanf.sscanf line "%d %d" (fun a b -> { a; b }) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range


let () =
    let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
    let edge_list = read_edges nr_edges in
    let graph = Graph.create nr_nodes edge_list in
    let result = Graph.bfs graph 1 in
    result |> List.map string_of_int |> String.concat " " |> print_endline
