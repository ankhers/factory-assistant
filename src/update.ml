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
  (* let graph = List.fold_left (fun acc part -> sum_parts acc part model.production_map) [] model.parts *)
  let graph = sum_parts model.parts model.production_map
  in
  { model with total_production = graph }

let update model =
  function
  | ChangePart (index, part) ->
    let parts = List.mapi (fun i (p, quantity) -> if i == index then (part, quantity) else (p, quantity)) model.parts
    in
    update_total_production { model with parts }
  | ChangeQuantity (index, quantity) ->
    let parts = List.mapi (fun i (part, q) -> if i == index then (part, quantity) else (part, q)) model.parts
    in
    update_total_production { model with parts }
  | AddPart ->
    let parts = List.append model.parts [(IronIngot, 30.)]
    in
    update_total_production { model with parts }
  | RemovePart index ->
    update_total_production { model with parts = remove_nth model.parts index }
