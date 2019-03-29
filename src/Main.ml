open Tea.App
open Types
open Model
open Update
open View

let init () = { part = IronIngot;
                number = 30;
                production_map = prod;
                production = Production.find IronIngot prod;
              }
(* MAIN *)
let main = beginnerProgram { model = (init ()); update; view }

(* EXAMPLE CONSOLE LOG *)
(* let _ =
 *   Js.log
 *     (("Hello, from BuckleScript and Reason!")[@reason.raw_literal
 *                                                "Hello, from BuckleScript and Reason!"]) *)
