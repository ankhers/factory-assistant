module Graphlib = struct
  module Graph = struct
    type t

    external create : t = "Graph"
    [@@bs.module "dagre-d3"] [@@bs.scope "graphlib"] [@@bs.new]

    external set_graph : t -> 'a Js.t -> unit = "setGraph"
    [@@bs.send]

    external set_node : t -> string -> 'a Js.t -> unit = "setNode"
    [@@bs.send]

    external set_edge : t -> string -> string -> 'a Js.t -> unit = "setEdge"
    [@@bs.send]

    external graph_height : t -> float = "graph.height"
    [@@bs.send]
  end
end

type 'a r = 'a Js.t -> Graphlib.Graph.t -> unit

external render : 'a r = "render"
[@@bs.module "dagre-d3"] [@@bs.new]
