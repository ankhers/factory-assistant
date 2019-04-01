open Tea.App
open Model
open Update
open View

(* MAIN *)
let main = beginnerProgram { model = init (); update; view; }

(* EXAMPLE CONSOLE LOG *)
(* let _ =
 *   Js.log
 *     (("Hello, from BuckleScript and Reason!")[@reason.raw_literal
 *                                                "Hello, from BuckleScript and Reason!"]) *)
