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

           expect received.parts |> toEqual expected.parts
         )
    )

let _ =
  describe "Graph Node Creation"
    (fun () ->
       let open Expect in

       test "initial" (fun () ->
           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot")] in

           expect received |> toEqual expected
         );

       test "two irons" (fun () ->
           let model = update model AddPart in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Ingot2", "Iron Ingot")] in

           expect received |> toEqual expected
         );

       test "two irons low output" (fun () ->
           let model = update model AddPart in
           let model = update model (ChangeQuantity (0, 1.)) in
           let model = update model (ChangeQuantity (1, 1.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot")] in

           expect received |> toEqual expected
         );

       test "screw full output" (fun () ->
           let model = update model (ChangePart (0, Screw)) in
           let model = update model (ChangeQuantity (0, 90.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Rod1", "Iron Rod"); ("Screw1", "Screw")] in

           expect received |> toEqual expected
         );

       test "screw perfect output" (fun () ->
           let model = update model (ChangePart (0, Screw)) in
           let model = update model (ChangeQuantity (0, 180.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Rod1", "Iron Rod"); ("Iron Rod2", "Iron Rod"); ("Screw1", "Screw"); ("Screw2", "Screw")] in

           expect received |> toEqual expected
         )
    )
