open Tea.App
open Tea.Html

(* MODEL *)
let init () = 4

(* UPDATE *)
type msg =
  | Increment
  | Decrement
  | Reset
  | Set of int[@@bs.deriving { accessors }]

let update model =
  function
  | Increment  -> model + 1
  | Decrement  -> model - 1
  | Reset  -> 0
  | Set v -> v

(* VIEW *)

(* Styles Definition Example *)
let view_button title msg = button [onClick msg] [text title]

let view model =
  div []
    [header []
       [h1 []
          [text "BuckleScript Tea Starter Kit"]];
    nav []
      [br [];
       view_button "Increment" Increment;
      (* view_button (("Increment")[@reason.raw_literal "Increment"]) Increment; *)
      br [];
      view_button "Decrement" Decrement;
      br [];
      view_button "Set to 42" (Set 42);
      br [];
      if model <> 0
      then view_button "Reset" Reset
      else noNode];
    div []
      [span [] [text (string_of_int model)]]]

(* MAIN *)
let main = beginnerProgram { model = (init ()); update; view }

(* EXAMPLE CONSOLE LOG *)
let _ =
  Js.log
    (("Hello, from BuckleScript and Reason!")[@reason.raw_literal
                                               "Hello, from BuckleScript and Reason!"])
