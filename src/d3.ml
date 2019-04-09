external select : string -> 'a Js.t = "select"
[@@bs.module "d3"]

external svg_select : 'a Js.t -> string -> 'a Js.t = "select"
[@@bs.send]

external svg_attr : 'a Js.t -> string -> float -> unit = "attr"
[@@bs.send]

external zoom : unit -> 'a Js.t = "zoom"
[@@bs.module "d3"]

external on : 'a Js.t -> string -> (unit -> 'a Js.t) -> 'a Js.t = "on"
[@@bs.send]

external attr : 'a Js.t -> string -> 'a Js.t -> 'a Js.t = "attr"
[@@bs.send]

external call : 'a Js.t -> 'a Js.t -> unit = "call"
[@@bs.send]

external call3 : 'a Js.t -> 'a Js.t -> 'a Js.t -> unit = "call"
[@@bs.send]

external event_transform : 'a Js.t = "transform"
[@@bs.module "d3"] [@@bs.scope "event"]

external transform : 'a Js.t -> 'a Js.t = "transform"
[@@bs.get]

external get_float : 'a Js.t -> string -> float = "attr"
[@@bs.send]

external zoomIdentity_translate : float -> float -> 'a Js.t = "translate"
[@@bs.module "d3"] [@@bs.scope "zoomIdentity"]

external set_attr : 'a Js.t -> string -> float -> unit = "attr"
[@@bs.send]

external node : 'a Js.t -> 'a Js.t = "node"
[@@bs.send]

external getBBox : 'a Js.t -> 'a Js.t = "getBBox"
[@@bs.send]

external width : 'a Js.t -> float = "width"
[@@bs.get]
