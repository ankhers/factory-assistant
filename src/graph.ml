open Types
open Types_encode
open Model

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rem_prod part quantity l =
  List.map (fun (p, q) ->
      if compare p part == 0
      then (p, q -. quantity)
      else (p, q)) l



let build_nodes model =
  let rec aux (total_prod, nodes) (part, quantity) =
    let parent_production = Production.find part model.production_map in
    let number_of_buildings = ceil @@ (quantity /. parent_production.output) in
    let (buildings, _) = List.fold_left (fun (nodes, quantity) _ ->
        let q =
          if quantity >= parent_production.output
          then parent_production.output
          else quantity
       in
        ((part, q, q /. parent_production.output) :: nodes, quantity -. q)
      ) ([], quantity) (1 -- (int_of_float number_of_buildings))
    in
    let total_prod = rem_prod part quantity total_prod in
    List.fold_left (fun acc (p, q) -> aux acc (p, q *. number_of_buildings) ) (total_prod, List.append nodes buildings) parent_production.input
  in
  List.fold_left aux (model.total_production, []) model.parts

let max_conveyor_speed tier =
  if tier >= 6
  then 270
  else if tier >= 3
  then 120
  else 60

let render model =
  let g = DagreD3.Graphlib.Graph.create in
  let _ = DagreD3.Graphlib.Graph.set_graph g (Js.Obj.empty ()) in
  let s = max_conveyor_speed model.tier in
  let (prod, nodes) = build_nodes model in
  (* let _ = Js.log (List.map (fun (a,b) -> (encode_part a, b)) prod) in
   * let _ = Js.log (List.map (fun (a,b,c) -> (encode_part a, b, c)) nodes) in *)
  (* let (_, nodes, edges, logistics) = build model s in *)
  (* let _ = make nodes edges logistics g in *)
  let svg = D3.select "svg" in
  let inner = D3.svg_select svg "g" in
  let render = DagreD3.render in
  let _ = render inner g in
  ()
