external select : string -> 'a Js.t = "select"
[@@bs.module "d3"]

external svg_select : 'a Js.t -> string -> 'a Js.t = "select"
[@@bs.send]

let initialScale = 0.75

external svg_attr : 'a Js.t -> string -> float -> unit = "attr"
[@@bs.send]

(*   var initialScale = 0.75;
 * svg.call(zoom.transform, d3.zoomIdentity.translate((svg.attr("width") - g.graph().width * initialScale) / 2, 20).scale(initialScale));
 *
 * svg.attr('height', g.graph().height * initialScale + 40); *)
