open Jest
open Types
open Types_encode
open Model
open Update

let model = init ()

let ab l =
  Js.log @@ Array.of_list @@ List.map (fun (a,b) -> (encode_part a, b)) l
let abc l =
  Js.log @@ Array.of_list @@ List.map (fun (a,b,c) -> (encode_part a, b,c)) l
let abcd l =
  Js.log @@ Array.of_list @@ List.map (fun (a,b,c,d) -> (encode_part a, b,c,d)) l

let _ =
  describe "build_nodes" (fun () ->
      let open Expect in

      test "IronIngot 30" (fun () ->
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 30., 1., 30.)] in

          expect received |> toEqual expected);

      test "IronIngot 60" (fun () ->
          let model = update model (ChangeQuantity (0, 60.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 30., 1., 30.); (IronIngot, 30., 1., 30.)] in

          expect received |> toEqual expected);

      test "Screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 90.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 15., 0.5, 15.); (IronRod, 15., 1., 15.); (Screw, 90., 1., 90.)] in

          expect received |> toEqual expected);

      test "Screw 180" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 180.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 30., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (Screw, 90., 1., 90.); (Screw, 90., 1., 90.)] in

          expect received |> toEqual expected);

      test "Rotor 6" (fun () ->
          let model = update model (ChangePart (0, Rotor)) in
          let model = update model (ChangeQuantity (0, 6.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 3., 0.1, 3.); (IronIngot, 7., 0.23333333333333334, 7.); (IronIngot, 30., 1., 15.); (IronRod, 3., 0.2, 3.); (IronRod, 7., 0.4666666666666667, 7.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (Screw, 42., 0.4666666666666667, 42.); (Screw, 90., 1., 90.); (Rotor, 6., 1., 6.)] in

          expect received |> toEqual expected);

      test "Rotor 12" (fun () ->
          let model = update model (ChangePart (0, Rotor)) in
          let model = update model (ChangeQuantity (0, 12.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 6., 0.2, 3.); (IronIngot, 14., 0.4666666666666667, 7.); (IronIngot, 30., 1., 15.); (IronIngot, 30., 1., 15.); (IronRod, 6., 0.4, 3.); (IronRod, 14., 0.9333333333333333, 7.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (Screw, 84., 0.9333333333333333, 42.); (Screw, 90., 1., 90.); (Screw, 90., 1., 90.); (Rotor, 6., 1., 6.); (Rotor, 6., 1., 6.)] in

          expect received |> toEqual expected);

      test "Rotor 18" (fun () ->
          let model = update model (ChangePart (0, Rotor)) in
          let model = update model (ChangeQuantity (0, 18.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 9., 0.3, 3.); (IronIngot, 21., 0.7, 7.); (IronIngot, 30., 1., 15.); (IronIngot, 30., 1., 15.); (IronIngot, 30., 1., 15.); (IronRod, 7., 0.4666666666666667, 7.); (IronRod, 9., 0.6, 3.); (IronRod, 14., 0.9333333333333333, 7.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (Screw, 42., 0.4666666666666667, 42.); (Screw, 84., 0.9333333333333333, 42.); (Screw, 90., 1., 90.); (Screw, 90., 1., 90.); (Screw, 90., 1., 90.); (Rotor, 6., 1., 6.); (Rotor, 6., 1., 6.); (Rotor, 6., 1., 6.)] in

          expect received |> toEqual expected);

      test "ModularFrame 4" (fun () ->
          let model = update model (ChangePart (0, ModularFrame)) in
          let model = update model (ChangeQuantity (0, 4.)) in
          let received = Graph.build_nodes model in
          let expected = [(IronIngot, 8., 0.26666666666666666, 8.); (IronIngot, 9., 0.3, 9.); (IronIngot, 10., 0.3333333333333333, 5.); (IronIngot, 15., 0.5, 15.); (IronIngot, 16., 0.5333333333333333, 16.); (IronIngot, 20., 0.6666666666666666, 10.); (IronIngot, 30., 1., 15.); (IronIngot, 30., 1., 30.); (IronIngot, 30., 1., 30.); (IronPlate, 8., 0.5333333333333333, 8.); (IronPlate, 10., 0.6666666666666666, 5.); (IronPlate, 15., 1., 15.); (IronPlate, 15., 1., 15.); (IronRod, 8., 0.5333333333333333, 8.); (IronRod, 9., 0.6, 9.); (IronRod, 10., 0.6666666666666666, 5.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (IronRod, 15., 1., 15.); (Screw, 48., 0.5333333333333333, 48.); (Screw, 60., 0.6666666666666666, 30.); (Screw, 90., 1., 90.); (Screw, 90., 1., 90.); (ReinforcedIronPlate, 2., 0.4, 2.); (ReinforcedIronPlate, 5., 1., 5.); (ReinforcedIronPlate, 5., 1., 5.); (ModularFrame, 4., 1., 4.)] in

          expect received |> toEqual expected);

      test "EncasedIndustrialBeam 4" (fun () ->
          let model = update model (ChangePart (0, EncasedIndustrialBeam)) in
          let model = update model (ChangeQuantity (0, 4.)) in
          let received = Graph.build_nodes model in
          let expected = [(SteelIngot, 18., 0.6, 18.); (SteelIngot, 30., 1., 30.); (Concrete, 5., 0.3333333333333333, 5.); (Concrete, 15., 1., 15.); (SteelBeam, 6., 0.6, 6.); (SteelBeam, 10., 1., 10.); (EncasedIndustrialBeam, 4., 1., 4.)] in

          expect received |> toEqual expected);

      test "EncasedIndustrialBeam 8" (fun () ->
          let model = update model (ChangePart (0, EncasedIndustrialBeam)) in
          let model = update model (ChangeQuantity (0, 8.)) in
          let received = Graph.build_nodes model in
          let expected = [(SteelIngot, 18., 0.6, 18.); (SteelIngot, 18., 0.6, 18.); (SteelIngot, 30., 1., 30.); (SteelIngot, 30., 1., 30.); (Concrete, 10., 0.6666666666666666, 5.); (Concrete, 15., 1., 15.); (Concrete, 15., 1., 15.); (SteelBeam, 6., 0.6, 6.); (SteelBeam, 6., 0.6, 6.); (SteelBeam, 10., 1., 10.); (SteelBeam, 10., 1., 10.); (EncasedIndustrialBeam, 4., 1., 4.); (EncasedIndustrialBeam, 4., 1., 4.)] in

          expect received |> toEqual expected);
    )

let _ =
  describe "build_edges" (fun () ->
      let open Expect in

      test "IronIngot 30" (fun () ->
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [] in
          let expected_logistics = [] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

      test "IronIngot 60" (fun () ->
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [] in
          let expected_logistics = [] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

       test "IronRod 15" (fun () ->
          let model = update model (ChangePart (0, IronRod)) in
          let model = update model (ChangeQuantity (0, 15.)) in
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [("Iron Ingot0", "Iron Rod1")] in
          let expected_logistics = [] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

       test "IronRod 30" (fun () ->
          let model = update model (ChangePart (0, IronRod)) in
          let model = update model (ChangeQuantity (0, 30.)) in
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [("Splitter0", "Iron Rod1"); ("Iron Ingot0", "Splitter0"); ("Splitter0", "Iron Rod2")] in
          let expected_logistics = [Splitter (IronIngot, 30., 2., 2.)] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

        test "Screw 90" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 90.)) in
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [("Iron Ingot0", "Iron Rod1"); ("Iron Rod1", "Screw2")] in
          let expected_logistics = [] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

       test "Screw 180" (fun () ->
          let model = update model (ChangePart (0, Screw)) in
          let model = update model (ChangeQuantity (0, 180.)) in
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [("Splitter0", "Iron Rod2"); ("Iron Rod2", "Screw3"); ("Iron Ingot0", "Splitter0"); ("Splitter0", "Iron Rod1"); ("Iron Rod1", "Screw4")] in
          let expected_logistics = [Splitter (IronIngot, 30., 2., 2.)] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

       test "EncasedIndustrialBeam 4" (fun () ->
          let model = update model (ChangePart (0, EncasedIndustrialBeam)) in
          let model = update model (ChangeQuantity (0, 4.)) in
          let nodes = Graph.build_nodes model in
          let received = Graph.build_edges model (Array.of_list nodes) in
          let expected_edges = [("Concrete2", "Merger1"); ("Concrete3", "Merger1"); ("Merger1", "Encased Industrial Beam6"); ("Steel Ingot1", "Steel Beam5"); ("Steel Ingot0", "Steel Beam4"); ("Steel Beam4", "Merger0"); ("Steel Beam5", "Merger0"); ("Merger0", "Encased Industrial Beam6")] in
          let expected_logistics = [Merger (SteelBeam, 16., 16.); Merger (Concrete, 20., 20.)] in
          let expected = (expected_edges, expected_logistics) in

          expect received |> toEqual expected);

       (* test "EncasedIndustrialBeam 8" (fun () ->
       *     let model = update model (ChangePart (0, EncasedIndustrialBeam)) in
       *     let model = update model (ChangeQuantity (0, 8.)) in
       *     let nodes = Graph.build_nodes model in
       *     let received = Graph.build_edges model (Array.of_list nodes) in
       *     let expected_edges = [("Splitter2", "Merger4"); ("Concrete6", "Merger4"); ("Merger4", "Encased Industrial Beam11"); ("Steel Ingot3", "Steel Beam10"); ("Steel Ingot1", "Steel Beam8"); ("Steel Beam8", "Merger3"); ("Steel Beam10", "Merger3"); ("Merger3", "Encased Industrial Beam11"); ("Concrete4", "Splitter2"); ("Splitter2", "Merger1"); ("Concrete5", "Merger1"); ("Merger1", "Encased Industrial Beam12"); ("Steel Ingot2", "Steel Beam9"); ("Steel Ingot0", "Steel Beam7"); ("Steel Beam7", "Merger0"); ("Steel Beam9", "Merger0"); ("Merger0", "Encased Industrial Beam12")] in
       *     let expected_logistics = [Merger (SteelBeam, 16., 16.); Merger (Concrete, 20., 20.); Splitter (Concrete, 10., 2, 2); Merger (SteelBeam, 16., 16.); Merger (Concrete, 20., 20.)] in
       *     let expected = (expected_edges, expected_logistics) in
       *
       *     expect received |> toEqual expected); *)

      (* test "ReinforcedIronPlate 5" (fun () ->
       *     let model = update model (ChangePart (0, ReinforcedIronPlate)) in
       *     let model = update model (ChangeQuantity (0, 5.)) in
       *     let nodes = Graph.build_nodes model in
       *     let received = Graph.build_edges model (Array.of_list nodes) in
       *     let expected_edges = [] in
       *     let expected_logistics = [] in
       *     let expected = (expected_edges, expected_logistics) in
       *
       *     expect received |> toEqual expected); *)
    )
