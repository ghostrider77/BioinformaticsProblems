module Node = struct
    type t = { original_id : int; nr_nodes : int; node_id : int }

    let compare {original_id = id1; _ } { original_id = id2; _ } =
        compare id1 id2

    let create original_id nr_nodes =
        let node_id = if original_id < 0 then -original_id + nr_nodes else original_id in
        { original_id; nr_nodes; node_id }

    let negate { original_id; nr_nodes; _ } =
        create (-original_id) nr_nodes

    let string_of_node { original_id; _ } =
        string_of_int original_id
end

module NodeMap = Map.Make(Node)

type edge = { a : Node.t; b : Node.t }
type component = Node.t list
type dfsresult = { components : component list; previsit_ids : int list; postvisit_ids : int list }

module Graph : sig
    type t
    val dfs : t -> dfsresult
    val create : int -> edge list -> Node.t list option -> t
    val node_order : t -> Node.t list
end = struct
    type t = { nr_nodes : int; adjacency_list : Node.t list NodeMap.t; ordered_nodes : Node.t list }

    let node_order { ordered_nodes; _ } = ordered_nodes

    let get_original_id nr_nodes node_id =
        let n = nr_nodes / 2 in
        if node_id <= n then node_id else -(node_id - n)

    let get_neighbors adjacency_list node =
        match NodeMap.find_opt node adjacency_list with
            | None -> []
            | Some neighbors -> neighbors

    let create nr_nodes edges node_order =
        let add_edge adjacency_list { a; b } =
            let update_function = function
                | None -> Some [b]
                | Some ns -> Some (b :: ns) in
            NodeMap.update a update_function adjacency_list in
        let adjacency_list = List.fold_left add_edge NodeMap.empty edges in
        let ordered_nodes = match node_order with
            | None -> List.init nr_nodes (fun k -> Node.create (get_original_id nr_nodes (k + 1)) (nr_nodes / 2))
            | Some nodes -> nodes in
        { nr_nodes; adjacency_list; ordered_nodes }

    let dfs { nr_nodes; adjacency_list; ordered_nodes } =
        let visit_started = Array.make nr_nodes 0 in
        let visit_ended = Array.make nr_nodes 0 in
        let previsit_id = ref 1 in
        let postvisit_id = ref 1 in
        let is_node_visited (node : Node.t) = visit_started.(node.node_id - 1) > 0 in
        let find_unvisited_neighbor node =
            node |> get_neighbors adjacency_list |> List.find_opt (fun item -> not (is_node_visited item)) in

        let explore (start_node : Node.t) =
            let rec traverse_component component = function
                | [] -> component
                | (node :: rest) as previsit_stack ->
                    match find_unvisited_neighbor node with
                        | Some neighbor ->
                            visit_started.(neighbor.node_id - 1) <- !previsit_id;
                            previsit_id := !previsit_id + 1;
                            traverse_component (neighbor :: component) (neighbor :: previsit_stack)
                        | None ->
                            visit_ended.(node.node_id - 1) <- !postvisit_id;
                            postvisit_id := !postvisit_id + 1;
                            traverse_component component rest in
            visit_started.(start_node.node_id - 1) <- !previsit_id;
            previsit_id := !previsit_id + 1;
            traverse_component [start_node] [start_node] in

        let rec find_components components = function
            | [] -> components
            | node :: remaining_nodes ->
                if is_node_visited node then find_components components remaining_nodes
                else
                    let current_component = explore node in
                    find_components (current_component :: components) remaining_nodes in

        let components = find_components [] ordered_nodes in
        { components; previsit_ids = Array.to_list visit_started; postvisit_ids = Array.to_list visit_ended }
end


let read_edges (nr_nodes : int) (nr_edges : int) : edge list =
    let parse_line line =
        let create_edges a b =
            let node1 = Node.create a nr_nodes in
            let node2 = Node.create b nr_nodes in
            [{ a = Node.negate node1; b = node2 }; { a = Node.negate node2; b = node1 }] in
        Scanf.sscanf line "%d %d" (fun a b -> List.to_seq (create_edges a b)) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.concat_map (fun _ -> parse_line (read_line ())) range


let read_graphs (n : int) : (int * edge list) list =
    let read_graph_data () =
        ignore (read_line ());
        let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
        let edges = read_edges nr_nodes nr_edges in
        (2*nr_nodes, edges) in
    List.(map (fun _ -> read_graph_data ()) @@ init n (fun _ -> 0))


let get_node_visit_order_in_reversed_graph (postvisit_ids : int list) (node_order : Node.t list) : Node.t list =
    let compare_pairs (_, id1) (_, id2) = compare id2 id1 in
    List.(postvisit_ids |> combine node_order |> sort compare_pairs |> map (fun (node, _) -> node ))


let calc_strongly_connected_components (nr_nodes : int) (edges : edge list) : component list =
    let graph = Graph.create nr_nodes edges None in
    let { postvisit_ids; _ } = Graph.dfs graph in
    let reversed_edges = List.map (fun {a; b} -> {a = b; b = a}) edges in
    let node_order = get_node_visit_order_in_reversed_graph postvisit_ids (Graph.node_order graph) in
    let reversed_graph = Graph.create nr_nodes reversed_edges (Some node_order) in
    let { components; _ } = Graph.dfs reversed_graph in
    components


let calc_component_assignment (component : component) (literal_assignment : Node.t list) : Node.t list option =
    let rec loop acc = function
        | [] -> Some acc
        | node :: rest ->
            if List.mem (Node.negate node) acc then None
            else if not (List.mem node literal_assignment) && not (List.mem (Node.negate node) literal_assignment) then
                loop (node :: acc) rest
            else loop acc rest in
    loop [] component


let solve_2sat (nr_nodes : int) (edges : edge list) : Node.t list =
    let rec loop acc = function
        | [] -> acc
        | component :: css ->
            match calc_component_assignment component acc with
                | None -> []
                | Some component_assignment -> loop (acc @ component_assignment) css in
    let components = calc_strongly_connected_components nr_nodes edges in
    loop [] components


let display_result (nodes : Node.t list) : unit =
    if nodes = [] then print_endline "0"
    else
        let compare_nodes (node1 : Node.t) (node2 : Node.t) =
            compare (abs node1.original_id) (abs node2.original_id) in
        let node_strings = List.(nodes |> sort compare_nodes |> map Node.string_of_node) in
        print_endline @@ String.concat " " ("1" :: node_strings)


let () =
    let nr_examples = read_int () in
    let graph_data = read_graphs nr_examples in
    let results = List.map (fun (nr_nodes, edges) -> solve_2sat nr_nodes edges) graph_data in
    List.iter display_result results
