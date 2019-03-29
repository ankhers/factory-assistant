open Types
open Types_encode
open Types_decode
open Msg
open Model
open Tea.Html

let view_button title msg = button [onClick msg] [text title]

let view_option v t = option' [ value v ] [ text t ]

let form_view model =
  div []
    [
      label [ for' "part" ] [ text "Part" ];
      select
        [
          onChange (fun part -> SetPart (decode_part part));
          id "part"
        ]
        [
          optgroup [ Vdom.prop "label" "Smelter" ]
            [
              view_option "iron_ingot" "Iron Ingot";
              view_option "copper_ingot" "Copper Ingot";
            ];

          optgroup [ Vdom.prop "label" "Constructor" ]
            [
              view_option "iron_plate" "Iron Plate";
              view_option "iron_rod" "Iron Rod";
              view_option "wire" "Wire";
              view_option "cable" "Cable";
              view_option "concrete" "Concrete";
              view_option "screw" "screw";
              view_option "steel_beam" "Steel Beam";
              view_option "steel_pipe" "Steel Pipe";
            ];

          optgroup [ Vdom.prop "label" "Assembler" ]
            [
              view_option "reinforced_iron_plate" "Reinforced Iron Plate";
              view_option "rotor" "Rotor";
              view_option "modular_frame" "Modular Frame";
              view_option "encased_industrial_beam" "EncasedIndustrialBeam";
              view_option "stator" "Stator";
              view_option "motor" "Motor";
            ];

          optgroup [ Vdom.prop "label" "Foundry" ]
            [
              view_option "steel_ingot" "Steel Ingot";
            ];
        ];
      br [];
      label [ for' "quantity" ] [ text "Quantity" ];
      input'
        [
          onInput (fun s -> ChangeNumber (float_of_string s));
          id "quantity";
          type' "number";
          value (Js.Float.toString model.number)
        ] [];
      br [];
      text (String.concat " " ["Max output per minute per building"; Js.Float.toString model.production.output]);
      hr [] [];
    ]

let output_view production part quantity =
  div []
    [
      text (String.concat " " [
        Js.Float.toString quantity;
        encode_building production.building;
        "-";
        encode_part part;
      ])
    ]

let rec input_view part number_of_buildings production_map =
  div [] [
    div [class' "col-lg-12"]
      (List.map (fun (dependency, num_required) ->
           let dep_production = Production.find dependency production_map in
           let modifier = ceil (num_required /. dep_production.output) in
           let quantity = modifier *. dep_production.output
           in
           [
             calculation_view dependency (quantity *. number_of_buildings) production_map
           ]) part.input |> List.concat);
  ]
and calculation_view part quantity production_map =
  let production = Production.find part production_map in
  let number_of_buildings = ceil (quantity /. production.output)
  in
  div []
    [
      output_view production part number_of_buildings;
      input_view production number_of_buildings production_map;
    ]

let view model =
  div []
    [
      h1 [] [ text "Factory Assistant" ];
      form_view model;
      calculation_view model.part model.number model.production_map;
    ]
