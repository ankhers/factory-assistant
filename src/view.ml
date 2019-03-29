open Types
open Types_encode
open Types_decode
open Msg
open Model
open Tea.Html

let view_button title msg = button [onClick msg] [text title]

let view_option v = option' [ value v ] [ text v ]

let form_view model =
  div []
    [
      div [ class' "form-group" ]
        [
          label [ for' "part" ] [ text "Part" ];
          select
            [
              onChange (fun part -> SetPart (decode_part part));
              id "part";
              class' "col-sm-12 form-control";
            ]
            [
              optgroup [ Vdom.prop "label" "Smelter" ]
                [
                  view_option (encode_part IronIngot);
                  view_option (encode_part CopperIngot);
                ];

              optgroup [ Vdom.prop "label" "Constructor" ]
                [
                  view_option (encode_part IronPlate);
                  view_option (encode_part IronRod);
                  view_option (encode_part Wire);
                  view_option (encode_part Cable);
                  view_option (encode_part Concrete);
                  view_option (encode_part Screw);
                  view_option (encode_part SteelBeam);
                  view_option (encode_part SteelPipe);
                ];

              optgroup [ Vdom.prop "label" "Assembler" ]
                [
                  view_option (encode_part ReinforcedIronPlate);
                  view_option (encode_part Rotor);
                  view_option (encode_part ModularFrame);
                  view_option (encode_part EncasedIndustrialBeam);
                  view_option (encode_part Stator);
                  view_option (encode_part Motor);
                ];

              optgroup [ Vdom.prop "label" "Foundry" ]
                [
                  view_option (encode_part SteelIngot);
                ];
            ];
        ];
      div [ class' "form-group" ]
        [
          label [ for' "quantity" ] [ text "Quantity" ];
          input'
            [
              onInput (fun s -> ChangeNumber (float_of_string s));
              id "quantity";
              type' "number";
              value (Js.Float.toString model.number);
              class' "form-control"
            ] [];
        ];
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
  div [class' "col-lg-6 offset-lg-3"]
    [
      h1 [] [ text "Factory Assistant" ];
      form_view model;
      calculation_view model.part model.number model.production_map;
    ]
