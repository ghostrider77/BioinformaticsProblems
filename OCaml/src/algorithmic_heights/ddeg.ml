module IntMap = Map.Make(Int)

type edge = { a : int; b : int }

module Graph : sig
    type t
    val create : int -> edge list -> t
    val get_neighbors : t -> int -> int list
    val degrees : t -> int IntMap.t
end = struct
    type t = { nr_nodes : int; adjacency_list : int list IntMap.t }

    let get_neighbors {adjacency_list; _} node =
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

    let degrees {adjacency_list; _} =
        IntMap.map List.length adjacency_list

end


let read_edges (nr_edges : int) : edge list =
    let parse_line line =
        Scanf.sscanf line "%d %d" (fun a b -> { a; b }) in
    let range = Seq.init nr_edges (fun k -> k) in
    List.of_seq @@ Seq.map (fun _ -> parse_line (read_line ())) range


let calc_double_degree_array (nr_nodes : int) (edges : edge list) : int list =
    let graph = Graph.create nr_nodes edges in
    let degrees = Graph.degrees graph in
    let get_degree node =
        match IntMap.find_opt node degrees with
            | None -> 0
            | Some d -> d in
    let nodes = List.init nr_nodes (fun k -> k + 1) in
    List.map (fun node -> List.fold_left (fun acc n -> acc + get_degree n) 0 @@ Graph.get_neighbors graph node) nodes


let () =
    let (nr_nodes, nr_edges) = Scanf.sscanf (read_line ()) "%d %d" (fun n e -> (n, e)) in
    let edge_list = read_edges nr_edges in
    let result = calc_double_degree_array nr_nodes edge_list in
    result |> List.map string_of_int |> String.concat " " |> print_endline
