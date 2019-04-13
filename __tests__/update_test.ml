open Jest
open Types
open Model
open Update

let model = init ()

let _ =
  describe "Model Update" (fun () ->
       let open Expect in

       test "ChangePart" (fun () ->
           let received = update model (ChangePart (0, CopperIngot)) in
           let expected = [(CopperIngot, 30.)] in

           expect received.parts |> toEqual expected);

       test "ChangeQuantity" (fun () ->
           let received = update model (ChangeQuantity (0, 31.)) in
           let expected = [(IronIngot, 31.)] in

           expect received.parts |> toEqual expected);

       test "AddPart" (fun () ->
           let model = update model (ChangePart (0, CopperIngot)) in
           let model = update model (ChangeQuantity (0, 31.)) in

           let received = update model AddPart in
           let expected = [(CopperIngot, 31.); (IronIngot, 30.)] in
           expect received.parts |> toEqual expected);

       test "RemovePart" (fun () ->
           let received = update model (RemovePart 0) in
           let expected = [] in

           expect received.parts |> toEqual expected);
    )

let _ =
  describe "Total Production" (fun () ->
      let open Expect in

      test "IronIngot full" (fun () ->
          let received = update_total_production model in
          let expected = [(IronIngot, 30.)] in

          expect received.total_production |> toEqual expected);

      test "Screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let received = update model (ChangeQuantity (0, 90.)) in
          let expected = [(IronIngot, 15.); (IronRod, 15.); (Screw, 90.)] in

          expect received.total_production |> toEqual expected);

      test "Screw 180" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let received = update model (ChangeQuantity (0, 180.)) in
          let expected = [(IronIngot, 30.); (IronRod, 30.); (Screw, 180.)] in

          expect received.total_production |> toEqual expected);

      test "ModularFrame 4" (fun () ->
          let model = update model (ChangePart (0, ModularFrame)) in
          let received = update model (ChangeQuantity (0, 4.)) in

          let expected = [(IronIngot, 210.); (IronPlate, 60.); (IronRod, 84.); (Screw, 360.); (ReinforcedIronPlate, 12.); (ModularFrame, 4.)] in

          expect received.total_production |> toEqual expected);
    )
