open Types
open Types_encode
open Types_decode
open Msg
open Model
open Tea.Html

let rec intersperse list el =
  match list with
  | [] | [ _ ]   -> list
  | x :: y :: tl -> x :: el :: intersperse (y::tl) el

let view_button title msg = button [onClick msg] [text title]

let view_option v = option' [ value v ] [ text v ]

let number_to_locale_string: (float -> string) = [%raw fun num -> "return num.toLocaleString()"]

let form_view index (part, quantity) production_map =
  let production = Production.find part production_map
  in
  div [ class' "form-row" ]
    [
      div [ class' "col" ]
        [
          label [ for' "part" ] [ text "Part" ];
          select
            [
              onChange (fun part -> ChangePart (index, (decode_part part)));
              id "part";
              class' "form-control";
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
      div [ class' "col" ]
        [
          label [ for' "quantity" ] [ text "PPM" ];
          input'
            [
              onInput (fun s -> ChangeQuantity (index, (float_of_string s)));
              id "quantity";
              type' "number";
              value (Js.Float.toString quantity);
              class' "form-control"
            ] [];
          small [ class' "form-text text-muted" ]
            [
              text (String.concat " " ["Max ppm per"; encode_building production.building.building; ":"; Js.Float.toString production.output])
            ]
        ];
      div [ class' "col col-sm-1"; style "margin-top" "32px" ]
        [
          button [ class' "btn btn-danger"; onClick (RemovePart index) ] [ i [ class' "fa fa-times" ] [ ] ]
        ];
    ]

let output_view production part number_of_buildings quantity_needed =
  let remainder = mod_float quantity_needed production.output
  in
  div []
    [
      text (String.concat " " [
          number_to_locale_string number_of_buildings;
          encode_building production.building.building;
          "-";
          encode_part part;
          String.concat "" ["("; number_to_locale_string remainder; " Extra)";]
        ])
    ]

let rec input_view part number_of_buildings production_map =
  div [] [
    div [class' "col-lg-12"]
      (List.map (fun (dependency, num_required) ->
           let quantity_needed = number_of_buildings *. num_required
           in
           [
             calculation_view (dependency, quantity_needed) production_map
           ]) part.input |> List.concat);
  ]
and calculation_view (part, quantity_needed) production_map =
  let production = Production.find part production_map in
  let number_of_buildings = ceil (quantity_needed /. production.output)
  in
  div []
    [
      output_view production part number_of_buildings quantity_needed;
      input_view production number_of_buildings production_map;
    ]

let view model =
  div [class' "col-lg-6 offset-3"]
    [
      h1 [ class' "" ] [ text "Factory Assistant" ];
      div [] (List.mapi (fun i part -> form_view i part model.production_map) model.parts);
      div [ class' "text-right" ]
        [
          button [
            class' "btn btn-primary ";
            onClick AddPart;
          ] [ i [ class' "fa fa-plus" ] [] ]
        ];
      div []
        (intersperse (List.map (fun part -> calculation_view part model.production_map) model.parts) (hr [] []))
      (* calculation_view model.part model.number model.production_map; *)
    ]
