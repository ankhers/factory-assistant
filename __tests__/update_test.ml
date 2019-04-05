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

           expect received.parts |> toEqual expected.parts);
    )

let _ =
  describe "Graph Node Creation" (fun () ->
       let open Expect in

       test "initial" (fun () ->
           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot")] in

           expect received |> toEqual expected);

       test "two irons" (fun () ->
           let model = update model AddPart in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Ingot2", "Iron Ingot")] in

           expect received |> toEqual expected);

       test "two irons low output" (fun () ->
           let model = update model AddPart in
           let model = update model (ChangeQuantity (0, 1.)) in
           let model = update model (ChangeQuantity (1, 1.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot")] in

           expect received |> toEqual expected);

       test "screw full output" (fun () ->
           let model = update model (ChangePart (0, Screw)) in
           let model = update model (ChangeQuantity (0, 90.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Rod1", "Iron Rod"); ("Screw1", "Screw")] in

           expect received |> toEqual expected);

       test "screw perfect output" (fun () ->
           let model = update model (ChangePart (0, Screw)) in
           let model = update model (ChangeQuantity (0, 180.)) in

           let received = build_nodes model.total_production model.production_map in
           let expected = [("Iron Ingot1", "Iron Ingot"); ("Iron Rod1", "Iron Rod"); ("Iron Rod2", "Iron Rod"); ("Screw1", "Screw"); ("Screw2", "Screw")] in

           expect received |> toEqual expected);
    )

let _ =
  describe "Graph Edge Creation" (fun () ->
      let open Expect in

      test "initial" (fun () ->
          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [] in

          expect received |> toEqual expected);

      test "iron rod" (fun () ->
          let model = update model (ChangePart (0, IronRod)) in
          let model = update model (ChangeQuantity (0, 15.)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [("Iron Ingot1", "Splitter0"); ("Splitter0", "Iron Rod1")] in

          expect received |> toEqual expected);

      test "screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 90.)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [("Iron Ingot1", "Splitter0"); ("Splitter0", "Iron Rod1"); ("Iron Rod1", "Screw1")] in

          expect received |> toEqual expected);

      test "screw 180" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 180.)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [("Splitter0", "Iron Rod1"); ("Iron Rod1", "Screw2"); ("Iron Ingot1", "Splitter0"); ("Splitter0", "Iron Rod2"); ("Iron Rod2", "Screw1")] in

          expect received |> toEqual expected);

      test "reinforced iron plate 5" (fun () ->
          let model = update model (ChangeQuantity (0, 5.)) in
          let model = update model (ChangePart (0, ReinforcedIronPlate)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [("Splitter2", "Iron Rod1"); ("Iron Rod1", "Screw2"); ("Iron Ingot1", "Splitter2"); ("Splitter2", "Iron Rod2"); ("Iron Rod2", "Screw1"); ("Screw2", "Merger1"); ("Screw1", "Merger1"); ("Merger1", "Reinforced Iron Plate1"); ("Iron Ingot2", "Iron Plate2"); ("Iron Ingot3", "Iron Plate1"); ("Iron Plate2", "Merger0"); ("Iron Plate1", "Merger0"); ("Merger0", "Reinforced Iron Plate1")] in

          expect received |> toEqual expected);

      test "rotor 6" (fun () ->
          let model = update model (ChangeQuantity (0, 6.)) in
          let model = update model (ChangePart (0, Rotor)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [("Splitter3", "Iron Rod1"); ("Iron Rod1", "Screw2"); ("Iron Ingot1", "Splitter3"); ("Splitter3", "Iron Rod2"); ("Iron Rod2", "Screw1"); ("Screw2", "Merger2"); ("Screw1", "Merger2"); ("Merger2", "Rotor1"); ("Splitter1", "Iron Rod4"); ("Iron Ingot2", "Splitter1"); ("Splitter1", "Iron Rod3"); ("Iron Rod4", "Merger0"); ("Iron Rod3", "Merger0"); ("Merger0", "Rotor1")] in

          expect received |> toEqual expected);

      test "modular frame 4" (fun () ->
          let model = update model (ChangeQuantity (0, 4.)) in
          let model = update model (ChangePart (0, ModularFrame)) in

          let (received, _) = build_edges model.parts model.total_production model.production_map in
          let expected = [] in

          expect received |> toEqual expected);
    )
