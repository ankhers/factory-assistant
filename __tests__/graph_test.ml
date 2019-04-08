open Jest
open Types
open Types_encode
open Model
open Update

let model = init ()

let _ =
  describe "build_nodes" (fun () ->
      let open Expect in

      test "IronIngot 30" (fun () ->
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.)] in
          let expected_nodes = [(IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "IronIngot 60" (fun () ->
          let model = update model (ChangeQuantity (0, 60.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.)] in
          let expected_nodes = [(IronIngot, 30., 1.); (IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "Screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 90.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.); (IronRod, 0.); (Screw, 0.)] in
          let expected_nodes = [(Screw, 90., 1.); (IronRod, 15., 1.); (IronIngot, 15., 0.5)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "Screw 180" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 180.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.); (IronRod, 0.); (Screw, 0.)] in
          let expected_nodes = [(Screw, 90., 1.); (Screw, 90., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "Rotor 6" (fun () ->
          let model = update model (ChangePart (0, Rotor)) in
          let model = update model (ChangeQuantity (0, 6.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.); (IronRod, 0.); (Screw, 0.); (Rotor, 0.)] in
          let expected_nodes = [(Rotor, 6., 1.); (IronRod, 3., 0.2); (IronRod, 15., 1.); (IronIngot, 30., 1.); (Screw, 42., 0.4666666666666667); (Screw, 90., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "Rotor 12" (fun () ->
          let model = update model (ChangePart (0, Rotor)) in
          let model = update model (ChangeQuantity (0, 12.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.); (IronRod, 0.); (Screw, 0.); (Rotor, 0.)] in
          let expected_nodes = [(Rotor, 6., 1.); (Rotor, 6., 1.); (IronRod, 6., 0.4); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronIngot, 15., 0.5); (IronIngot, 30., 1.); (Screw, 84., 0.9333333333333333); (Screw, 90., 1.); (Screw, 90., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronIngot, 15., 0.5); (IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);

      test "ModularFrame 6" (fun () ->
          let model = update model (ChangePart (0, ModularFrame)) in
          let model = update model (ChangeQuantity (0, 4.)) in
          let received = Graph.build_nodes model in
          let expected_production = [(IronIngot, 0.); (IronPlate, 0.); (IronRod, 0.); (Screw, 0.); (ReinforcedIronPlate, 0.); (ModularFrame, 0.)] in
          let expected_nodes = [(ModularFrame, 4., 1.); (ReinforcedIronPlate, 2., 0.4); (ReinforcedIronPlate, 5., 1.); (ReinforcedIronPlate, 5., 1.); (IronPlate, 15., 1.); (IronPlate, 15., 1.); (IronPlate, 15., 1.); (IronPlate, 15., 1.); (IronIngot, 30., 1.); (IronIngot, 30., 1.); (IronIngot, 30., 1.); (IronIngot, 30., 1.); (Screw, 90., 1.); (Screw, 90., 1.); (Screw, 90., 1.); (Screw, 90., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronRod, 15., 1.); (IronIngot, 30., 1.); (IronIngot, 30., 1.); (IronRod, 9., 0.6); (IronRod, 15., 1.); (IronIngot, 30., 1.)] in
          let expected = (expected_production, expected_nodes) in

          expect received |> toEqual expected);
    )

let _ =
  describe "build_edges" (fun () ->
      let open Expect in

      test "IronIngot 30" (fun () ->
          let (_, nodes) = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected = ([], []) in

          expect received |> toEqual expected);

      test "IronIngot 60" (fun () ->
          let (_, nodes) = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected = ([], []) in

          expect received |> toEqual expected);

      test "IronRod 15" (fun () ->
          let model = update model (ChangePart (0, IronRod)) in
          let model = update model (ChangeQuantity (0, 15.)) in
          let (_, nodes) = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected = ([("Iron Ingot1", "Iron Rod0")], []) in

          expect received |> toEqual expected);

      test "IronRod 30" (fun () ->
          let model = update model (ChangePart (0, IronRod)) in
          let model = update model (ChangeQuantity (0, 30.)) in
          let (_, nodes) = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected = ([("Splitter0", "Iron Rod0"); ("Iron Ingot2", "Splitter0"); ("Splitter0", "Iron Rod1")], [Splitter (IronIngot, 30., 2, 2)]) in

          expect received |> toEqual expected);

      test "Screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 90.)) in
          let (_, nodes) = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected = ([("Iron Ingot2", "Iron Rod1"); ("Iron Rod1", "Screw0")], []) in

          expect received |> toEqual expected);
    )
