open Types
open Types_encode
open Model

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rec change_nth n a =
  function
  | x :: xs -> if n == 0 then a :: xs else x :: change_nth (n - 1) a xs
  | _ -> assert false

let rem_prod part quantity l =
  List.map (fun (p, q) ->
      if compare p part == 0
      then (p, q -. quantity)
      else (p, q)) l

let find_nth pred l =
  let rec aux n =
    function
    | logistic :: l ->
      if pred logistic
      then (n, Some logistic)
      else aux (n + 1) l
    | [] -> (-1, None)
  in
  aux 0 l

let nth pred a =
  let rec aux n max =
    let (p, a, e) = a.(n) in
    if pred p a e
    then n
    else aux (n + 1) max
  in
  aux 0 (Array.length a - 1)

let build_edges model nodes =
  let rec parent_aux (edges, logistics, nodes) (part, quantity) =
    let parent_production = Production.find part model.production_map in
    let n = int_of_float @@ ceil @@ quantity /. parent_production.output in
    let (parent_nodes, _) = List.fold_left (fun (ns, q) _ ->
        let n = nth (fun p a _e ->
            p == part && a > 0.
          ) nodes
        in
        let ns = (encode_part part ^ string_of_int n) :: ns in
        let (p, a, e) = nodes.(n) in
        let _ = Array.set nodes n (p, a -. quantity, e) in
        (ns, q)
      ) ([], quantity) (1 -- n)
    in
    let zipped = List.map (fun a -> List.map (fun b -> (a, b)) parent_production.input) parent_nodes |> List.concat in
    let rec child_aux (edges, logistics, nodes) (parent_node, (part, quantity)) =
      let nodes_copy = Array.copy nodes in
      let child_production = Production.find part model.production_map in
      let n = int_of_float @@ ceil @@ quantity /. child_production.output in
      let (edges, logistics, _) = List.fold_left (fun (es, ls, q) _ ->
          let (ln, l) = find_nth (fun l ->
              match l with
              | Splitter (p, i, a, _c) ->
                p == part && i /. (float_of_int a) == q
              | _ -> assert false
            ) logistics
          in
          match l with
          | None ->
            let n = nth (fun p a _e ->
                let goal =
                  if child_production.output >= q
                  then q
                  else child_production.output
                in
                let m = a /. q in
                p == part && (a == goal || m == 2. || m == 3.)
              ) nodes
            in
            let (p, a, e) = nodes_copy.(n) in
            let _ = Array.set nodes_copy n (p, 0., e) in
            let part_name = encode_part part ^ string_of_int n in
            let (es, ls) =
              if a == q
              then ((encode_part part ^ string_of_int n, parent_node) :: es, ls)
              else
                let l =
                  if a < q
                  then Merger (part, 2, 1)
                  else Splitter (part, a, 2, 1) in
                let m = List.length ls in
                let ls = List.append ls [l] in
                let logistic_name = encode_logistic l ^ string_of_int m in
                let es = (part_name, logistic_name) :: (logistic_name, parent_node) :: es in
                (es, ls)
            in
            (es, ls, q)
          | Some l ->
            match l with
            | Splitter (p, i, a, c) ->
              let l = Splitter (p, i, a, c + 1) in
              let ls = change_nth ln l ls in
              let logistic_name = encode_logistic l ^ string_of_int ln in
              let es = (logistic_name, parent_node) :: es in
              (es, ls, q)
            | _ -> assert false
        ) (edges, logistics, quantity) (1 -- n)
      in
      parent_aux (edges, logistics, nodes) (part, quantity)
    in
    List.fold_left child_aux (edges, logistics, nodes) zipped
  in
  let (edges, logistics, _) =
    List.fold_left parent_aux ([], [], nodes) model.parts
  in
  (edges, logistics)

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

let make nodes edges logistics graph =
  let _ = List.mapi (fun i (p, _, _) ->
      let label = encode_part p in
      let attrs = [%bs.obj { label = label }] in
      DagreD3.Graphlib.Graph.set_node graph (label ^ string_of_int i) attrs
    ) nodes
  in
  let _ = List.mapi (fun i l ->
      let label = encode_logistic l in
      let attrs = [%bs.obj { label = label; shape = "diamond" }] in
      DagreD3.Graphlib.Graph.set_node graph (label ^ string_of_int i) attrs
    ) logistics
  in
  let _ = List.map (fun (child, parent) ->
      DagreD3.Graphlib.Graph.set_edge graph child parent (Js.Obj.empty ())
    ) edges
  in
  ()

let render model =
  let g = DagreD3.Graphlib.Graph.create in
  let _ = DagreD3.Graphlib.Graph.set_graph g (Js.Obj.empty ()) in
  (* let s = max_conveyor_speed model.tier in *)
  let (_, nodes) = build_nodes model in
  (* let (edges, logistics) = build_edges model (Array.of_list nodes) in
   * let _ = make nodes edges logistics g in *)
  let svg = D3.select "svg" in
  let inner = D3.svg_select svg "g" in
  let render = DagreD3.render in
  let _ = render inner g in
  ()
