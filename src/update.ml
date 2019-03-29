open Types
open Msg
open Model

let update model =
  function
  | SetPart part -> { model with part; production = Production.find part  model.production_map}
  | ChangeNumber number -> { model with number }
