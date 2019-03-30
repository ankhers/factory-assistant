open Types
open Msg
open Model

let rec remove_nth l n =
  match l with
  | x :: xs -> if n = 0 then remove_nth xs (n - 1) else x :: remove_nth xs (n - 1)
  | [] -> []

let update model =
  function
  | ChangePart (index, part) ->
    let parts = List.mapi (fun i (p, quantity) -> if i == index then (part, quantity) else (p, quantity)) model.parts
    in
    { model with parts }
  | ChangeQuantity (index, quantity) ->
    let parts = List.mapi (fun i (part, q) -> if i == index then (part, quantity) else (part, q)) model.parts
    in
    { model with parts }
  | AddPart ->
    let parts = List.append model.parts [(IronIngot, 30.)]
    in
    { model with parts }
  | RemovePart index ->
    { model with parts = remove_nth model.parts index }
