module IntMap = Map.Make(Int)

type edge = { a : int; b : int }


module Graph : sig
    type t
    val adjacency_list : t -> int list IntMap.t
    val topological_sorting : t -> int list
    val create : int -> edge list -> t
end = struct
    type t = { nr_nodes : int; adjacency_list : int list IntMap.t }

    let adjacency_list { adjacency_list; _ } = adjacency_list

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

    let topological_sorting { nr_nodes; adjacency_list } =
        let visit_started = Array.make nr_nodes 0 in
        let visit_ended = Array.make nr_nodes 0 in
        let topological_ordering = Queue.create () in
        let previsit_id = ref 1 in
        let postvisit_id = ref 1 in
        let is_node_visited node = visit_started.(node - 1) > 0 in
        let find_unvisited_neighbor node =
            node |> get_neighbors adjacency_list |> List.find_opt (fun item -> not (is_node_visited item)) in

        let explore start_node =
            let rec traverse_component component = function
                | [] -> component
                | (node :: rest) as previsit_stack ->
                    match find_unvisited_neighbor node with
                        | Some neighbor ->
                            visit_started.(neighbor - 1) <- !previsit_id;
                            previsit_id := !previsit_id + 1;
                            traverse_component (neighbor :: component) (neighbor :: previsit_stack)
                        | None ->
                            visit_ended.(node - 1) <- !postvisit_id;
                            postvisit_id := !postvisit_id + 1;
                            Queue.add node topological_ordering;
                            traverse_component component rest in
            visit_started.(start_node - 1) <- !previsit_id;
            previsit_id := !previsit_id + 1;
            traverse_component [start_node] [start_node] in

        let rec find_components components = function
            | [] -> components
            | node :: remaining_nodes ->
                if is_node_visited node then find_components components remaining_nodes
                else
                    let current_component = explore node in
                    find_components (current_component :: components) remaining_nodes in
            ignore (find_components [] (List.init nr_nodes (fun k -> k + 1)));
            topological_ordering |> Queue.to_seq |> List.of_seq |> List.rev
end


let read_edges (nr_edges : int) : edge list =
    let parse_line line =
        Scanf.sscanf line "%d %d" (fun a b -> { a; b }) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range


let () =
    let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
    let edges = read_edges nr_edges in
    let graph = Graph.create nr_nodes edges in
    let result = Graph.topological_sorting graph in
    result |> List.map string_of_int |> String.concat " " |> print_endline
