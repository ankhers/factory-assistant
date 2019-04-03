open Jest
open Types
open Model
open Update

let model = init ()

let _ =
  describe "Model Update"
    (fun () ->
       let open Expect in
       test "ChangePart" (fun () ->
           let received = update model (ChangePart (0, CopperIngot)) in
           let expected = { model with parts = [(CopperIngot, 30.)]} in

           expect received.parts |> toEqual expected.parts);

       test "ChangeQuantity" (fun () ->
           let received = update model (ChangeQuantity (0, 31.)) in
           let expected = { model with parts = [(IronIngot, 31.)]} in

           expect received.parts |> toEqual expected.parts);

       test "AddPart" (fun () ->
           let model = update model (ChangePart (0, CopperIngot)) in
           let model = update model (ChangeQuantity (0, 31.)) in
           let received = update model AddPart in
           let expected = { model with parts = [(CopperIngot, 31.); (IronIngot, 30.)]} in
           expect received.parts |> toEqual expected.parts);

       test "RemovePart" (fun () ->
           let received = update model (RemovePart 0) in
           let expected = { model with parts = [] } in

           expect received.parts |> toEqual expected.parts))
