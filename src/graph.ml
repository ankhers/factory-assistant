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
    | x :: l ->
      if pred x
      then (n, Some x)
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
    let (parent_nodes, _) = List.fold_left (fun (ns, quantity) _ ->
        let q =
          if quantity >= parent_production.output
          then parent_production.output
          else quantity
        in
        let n = nth (fun p a _e ->
            let m = a /. q in
            p == part && (a == q || m == 2. || m == 3.)
          ) nodes
        in
        let parent_efficiency = q /. parent_production.output in
        let ns = (encode_part part ^ string_of_int n, parent_efficiency) :: ns in
        let (p, a, e) = nodes.(n) in
        let _ = Array.set nodes n (p, a -. quantity, e) in
        (ns, quantity -. q)
      ) ([], quantity) (1 -- n)
    in
    let zipped = List.map (fun a -> List.map (fun b -> (a, b)) parent_production.input) parent_nodes |> List.concat in
    let child_aux (edges, logistics, nodes) ((parent_node, parent_efficiency), (part, quantity)) =
      let quantity = parent_efficiency *. quantity in
      let nodes_copy = Array.copy nodes in
      let child_production = Production.find part model.production_map in
      let n = int_of_float @@ ceil @@ quantity /. child_production.output in
      let (edges, logistics, _) = List.fold_left (fun (es, ls, q) _ ->
          let (ln, l) = find_nth (fun l ->
              match l with
              | Splitter (p, i, a, c) ->
                p == part && i /. (float_of_int a) == q && a > c
              | Merger (p, g, c) ->
                let m = q /. 2. in
                p == part && (c +. m == g || c +. q == g)
            ) ls
          in
          match l with
          | None ->
            let n = nth (fun p a _e ->
                let goal =
                  if child_production.output >= q
                  then q
                  else child_production.output
                in
                let m = a /. goal in
                p == part && (a == goal || m == 2. || m == 3.)
              ) nodes
            in
            let (p, a, e) = nodes_copy.(n) in
            let _ = Array.set nodes_copy n (p, 0., e) in
            let part_name = encode_part part ^ string_of_int n in
            let (es, ls) =
              if a == q
              then
                ((encode_part part ^ string_of_int n, parent_node) :: es, ls)
              else
                let l =
                  if a < q
                  then Merger (part, q, a)
                  else Splitter (part, a, 2, 1) in
                let m = List.length ls in
                let ls = List.append ls [l] in
                let logistic_name = encode_logistic l ^ string_of_int m in
                let es = (part_name, logistic_name) :: (logistic_name, parent_node) :: es in
                (es, ls)
            in
            let q = q -. a in
            (es, ls, q)
          | Some l ->
            match l with
            | Splitter (p, i, a, c) ->
              let l = Splitter (p, i, a, c + 1) in
              let ls = change_nth ln l ls in
              let logistic_name = encode_logistic l ^ string_of_int ln in
              let (es, ls) =
                if (float_of_int a) == q
                then ((logistic_name, parent_node) :: es, ls)
                else
                  let (n, l) = find_nth (fun l ->
                      match l with
                      | Splitter _ -> false
                      | Merger (p, g, c) ->
                        p == part && c +. q == g
                    ) ls
                  in
                  match l with
                  | None ->
                    ((logistic_name, parent_node) :: es, ls)
                  | Some Splitter _ -> assert false
                  | Some Merger (p,g,c) ->
                    let l = Merger (p, g, c +. q) in
                    let ls = change_nth n l ls in
                    let es = (logistic_name, encode_logistic l ^ string_of_int n) :: es in
                  (es, ls)
              in
              (es, ls, (float_of_int a) -. q)
            | Merger (p, g, c) ->
              let l = Merger (p, g, c +. q) in
              let ls = change_nth ln l ls in
              let logistic_name = encode_logistic l ^ string_of_int ln in
              let n = nth (fun p a _e ->
                  let goal =
                    if child_production.output >= q
                    then q
                    else child_production.output
                  in
                  let m = a /. q in
                  p == part && (a == goal || m == 2.)
                ) nodes
              in
              let (p, a, e) = nodes_copy.(n) in
              let _ = Array.set nodes_copy n (p, 0., e) in
              let part_name = encode_part part ^ string_of_int n in
              let (es, ls, q) =
                if a == q
                then ((part_name, logistic_name) :: es, ls, 0.)
                else
                  let splitter = Splitter (p, a, 2, 1) in
                  let m = List.length ls in
                  let ls = List.append ls [splitter] in
                  let splitter_name = encode_logistic splitter ^ string_of_int m in
                  let es = (part_name, splitter_name) :: (splitter_name, logistic_name) :: es in
                  (es, ls, a -. q)
              in
              (es, ls, q)
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
  let rec aux nodes (part, quantity) =
    let parent_production = Production.find part model.production_map in
    let number_of_buildings = ceil @@ quantity /. parent_production.output in
    let (nodes, _, inputs) = List.fold_left (fun (nodes, quantity, inputs) _ ->
        let q =
          if quantity > parent_production.output
          then parent_production.output
          else quantity
        in
        let (n, elem) = find_nth (fun (p, a, _) ->
            p == part && a +. q <= parent_production.output
            && (a == q || a /. 2. == q)
          ) nodes
        in
        match elem with
        | None ->
          let e = q /. parent_production.output in
          let nodes = (part, q, e) :: nodes in
          let is = List.map (fun (p, q) -> (p, e *. q)) parent_production.input in
          (nodes, quantity -. q, List.append is inputs)
        | Some (p, a, _) ->
          let e = q /. parent_production.output in
          let is = List.map (fun (p, q) -> (p, e *. q)) parent_production.input in
          let a = a +. q in
          let e = a /. parent_production.output in
          let nodes = change_nth n (p, a, e) nodes in
          (nodes, quantity -. q, List.append is inputs)
      ) (nodes, quantity, []) (1 -- (int_of_float number_of_buildings))
    in
    List.fold_left aux nodes inputs
  in
  List.fold_left aux [] model.parts

let max_conveyor_speed tier =
  if tier >= 6
  then 270
  else if tier >= 3
  then 120
  else 60

let make nodes edges logistics graph =
  let _ = List.mapi (fun i (p, a, _) ->
      let label = encode_part p in
      let attrs = [%bs.obj { label = label ^ " " ^ Js.Float.toString a }] in
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

let initial_scale = 1.25

let render model =
  let g = DagreD3.Graphlib.Graph.create in
  let _ = DagreD3.Graphlib.Graph.set_graph g (Js.Obj.empty ()) in
  (* let s = max_conveyor_speed model.tier in *)
  let nodes = build_nodes model in
  let (edges, logistics) = build_edges model (Array.of_list nodes) in
  let _ = make nodes edges logistics g in
  let svg = D3.select "svg" in
  let inner = D3.svg_select svg "g" in
  let zoom = D3.zoom () in
  let f = (fun () -> inner |. D3.attr "transform" D3.event_transform) in
  let zoom = zoom |. D3.on "zoom" f in
  let _ = D3.call svg zoom in
  let render = DagreD3.render in
  let _ = render inner g in
  let svg_width = svg |. D3.node |. D3.getBBox |. D3.width in
  let graph_width = DagreD3.Graphlib.Graph.graph g
                    |. DagreD3.Graphlib.Graph.width in
  let graph_height = DagreD3.Graphlib.Graph.graph g
                     |. DagreD3.Graphlib.Graph.height in
  let one = svg_width -. graph_width *. initial_scale /. 2. in
  let two = 20. in
  let translate = D3.zoomIdentity_translate one two in
  let _ = D3.call3 svg (D3.transform zoom) translate in
  let _ = D3.set_attr svg "height" (graph_height *. initial_scale +. 40.) in
  ()
