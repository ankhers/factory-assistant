open Types
open Types_encode
open Msg
open Model

let rec remove_nth l n =
  match l with
  | x :: xs -> if n = 0 then remove_nth xs (n - 1) else x :: remove_nth xs (n - 1)
  | [] -> []

let add_part list (part, quantity) =
  match List.filter (fun (p, _) -> p == part) list with
  | [] -> (part, quantity) :: list
  | [(_, _)] -> List.map (fun (p, q) -> if p == part then (part, q +. quantity) else (p, q)) list
  | _ -> assert false

let sum_parts parts production_map =
  let rec sum_parts_aux acc (part, quantity) =
    let production = Production.find part production_map in
    let number_of_buildings = ceil @@ quantity /. production.output in
    let deps = List.map (fun (part, quantity) -> (part, quantity *. number_of_buildings)) production.input
    in
    (part, quantity) :: List.fold_left sum_parts_aux acc deps
  in
  List.fold_left sum_parts_aux [] parts

let update_total_production model =
  let graph = sum_parts model.parts model.production_map in
  let graph = List.fold_left (fun acc part -> add_part acc part) [] graph
  in
  { model with total_production = graph }

(* let g = DagreD3.Graphlib.Graph.create
 *
 * let _ = DagreD3.Graphlib.Graph.set_graph g (Js.Obj.empty ())
 *
 * let _ = DagreD3.Graphlib.Graph.set_node g "Node 1" [%bs.obj { label = "Node1"}]
 * let _ = DagreD3.Graphlib.Graph.set_node g "Node 2" [%bs.obj { label = "Node2"}]
 *
 * let svg = D3.select "svg"
 * let inner = D3.svg_select svg "g"
 *
 * let render = DagreD3.render
 *
 * let _ = render inner g *)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let make_part_nodes (part, quantity) production_map =
  let production = Production.find part production_map in
  let range = 1 -- (int_of_float @@ ceil @@ quantity /. production.output) in
  let part = encode_part part in
  List.map (fun i -> (part ^ string_of_int i, part)) range

let build_nodes parts production_map =
  List.map (fun part -> make_part_nodes part production_map) parts
  |> List.flatten

let make_nodes model graph =
  build_nodes model.total_production model.production_map
  |> List.map (fun (node, label) ->
      let label = [%bs.obj { label = label }] in
      DagreD3.Graphlib.Graph.set_node graph node label
    )

let rem_production part quantity list =
  List.map (fun (p, q) -> if p == part then (p, q -. quantity) else (p, q)) list

let node_names part output number list =
  match List.filter (fun (p, _) -> p == part) list with
  | [(_, q)] ->
    let n = int_of_float (ceil @@ q /. output) in
    let number = int_of_float number in
    let start = n - number + 1 in
    List.map (fun i -> encode_part part ^ string_of_int i) (start -- n)
  | _ -> assert false

let rec make_edges_for acc (part, quantity) production_map graph =
  let parent_production = Production.find part production_map in
  let number_of_buildings = ceil @@ quantity /. parent_production.output in
  let parent_nodes = node_names part parent_production.output number_of_buildings acc in
  let acc = rem_production part quantity acc in
  let zipped = List.map (fun a -> List.map (fun b -> (a, b)) parent_production.input) parent_nodes |> List.concat in
  let aux acc (parent_node, (part, quantity)) =
    let production = Production.find part production_map in
    let number_of_buildings = ceil @@ quantity /. production.output in
    let nodes = node_names part production.output number_of_buildings acc in
    let _ = List.map (fun node ->
        DagreD3.Graphlib.Graph.set_edge graph node parent_node (Js.Obj.empty ())) nodes in
    List.fold_left (fun acc part -> make_edges_for acc part production_map graph) acc [(part, quantity)]
  in
  List.fold_left aux acc zipped

let make_edges model graph =
  List.fold_left (fun acc part -> make_edges_for acc part model.production_map graph) model.total_production model.parts


let render_graph model =
  let g = DagreD3.Graphlib.Graph.create in
  let _ = DagreD3.Graphlib.Graph.set_graph g (Js.Obj.empty ()) in
  let _ = make_nodes model g in
  let _ = make_edges model g in
  let svg = D3.select "svg" in
  let inner = D3.svg_select svg "g" in
  let render = DagreD3.render in
  let _ = render inner g in
  model

let update model =
  function
  | ChangePart (index, part) ->
    let parts = List.mapi (fun i (p, quantity) -> if i == index then (part, quantity) else (p, quantity)) model.parts in
    let model = update_total_production { model with parts } in
    let _ = render_graph model
    in
    model
  | ChangeQuantity (index, quantity) ->
    let parts = List.mapi (fun i (part, q) -> if i == index then (part, quantity) else (part, q)) model.parts in
    let model = update_total_production { model with parts } in
    let _ = render_graph model
    in
    model
  | AddPart ->
    let parts = List.append model.parts [(IronIngot, 30.)] in
    let model = update_total_production { model with parts } in
    let _ = render_graph model
    in
    model
  | RemovePart index ->
    let parts = remove_nth model.parts index in
    let model = update_total_production { model with parts } in
    let _ = render_graph model
    in
    model
