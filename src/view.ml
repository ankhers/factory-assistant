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
          onInput (fun s -> ChangeNumber (int_of_string s));
          id "quantity";
          type' "number";
          value (string_of_int model.number)
        ] [];

    ]

let output_view model =
  div []
    [
      text (string_of_int model.production.output)
    ]

let input_view model =
  div []
    (List.map (fun (part, quantity) -> [text (encode_part part); text " "; text (string_of_int quantity); br []]) model.production.input |> List.concat)

let calculation_view model =
  div []
    [
      output_view model;
      input_view model;
    ]

let view model =
  div []
    [
      h1 [] [ text "Factory Assistant" ];
      form_view model;
      calculation_view model;
    ]
