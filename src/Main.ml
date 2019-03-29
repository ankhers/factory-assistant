open Tea.App
open Tea.Html

type production_building =
  | Smelter
  | Constructor
  | Assembler
  | Foundry

type part =
  | IronIngot
  | CopperIngot

  | IronPlate
  | IronRod
  | Wire
  | Cable
  | Concrete
  | Screw
  | SteelBeam
  | SteelPipe

  | ReinforcedIronPlate
  | Rotor
  | ModularFrame
  | EncasedIndustrialBeam
  | Stator
  | Motor

  | SteelIngot

module Production = Map.Make(struct type t = part let compare = compare end)

(* MODEL *)
type production =
  { building : production_building;
    output : int;
    input : (part * int) list;
  }
let prod =
  Production.empty
    |> Production.add IronIngot { building = Smelter;
                                  output = 30;
                                  input = [] }
    |> Production.add CopperIngot { building = Smelter;
                                    output = 30;
                                    input = [] }

    |> Production.add IronPlate { building = Constructor;
                                  output = 15;
                                  input = [(IronIngot, 30)] }
    |> Production.add IronRod { building = Constructor;
                                output = 15;
                                input = [(IronIngot, 15)] }
    |> Production.add Wire { building = Constructor;
                             output = 45;
                             input = [(CopperIngot, 15)] }
    |> Production.add Cable { building = Constructor;
                              output = 15;
                              input = [(Wire, 30)] }
    |> Production.add Concrete { building = Constructor;
                                 output = 15;
                                 input = [] }
    |> Production.add Screw { building = Constructor;
                              output = 90;
                              input = [(IronRod, 15)] }
    |> Production.add SteelBeam { building = Constructor;
                                  output = 10;
                                  input = [(SteelIngot, 30)] }
    |> Production.add SteelPipe { building = Constructor;
                                  output = 15;
                                  input = [(SteelIngot, 15)] }

    |> Production.add ReinforcedIronPlate { building = Assembler;
                                            output = 5;
                                            input = [(IronPlate, 20); (Screw, 120)] }
    |> Production.add Rotor { building = Assembler;
                              output = 6;
                              input = [(IronRod, 16); (Screw, 132)] }
    |> Production.add ModularFrame { building = Assembler;
                                     output = 4;
                                     input = [(ReinforcedIronPlate, 12); (IronRod, 24)] }
    |> Production.add EncasedIndustrialBeam { building = Assembler;
                                              output = 4;
                                              input = [(SteelBeam, 16); (Concrete, 20)] }
    |> Production.add Stator { building = Assembler;
                               output = 6;
                               input = [(SteelPipe, 18); (Wire, 60)] }
    |> Production.add Motor { building = Assembler;
                              output = 5;
                              input = [(Rotor, 10); (Stator, 10)] }

    |> Production.add SteelIngot { building = Foundry;
                                   output = 30;
                                   input = [] }


type model =
  { part : part;
    number : int;
    production_map : production Production.t;
    production : production;
  }
let init () = { part = IronIngot;
                number = 30;
                production_map = prod;
                production = Production.find IronIngot prod;
              }

(* UPDATE *)
type msg =
  | SetPart of part
  | ChangeNumber of int

let update model =
  function
  | SetPart part -> { model with part; production = Production.find part  model.production_map}
  | ChangeNumber number -> { model with number }

(* VIEW *)
let view_button title msg = button [onClick msg] [text title]

let view_option v t = option' [ value v ] [ text t ]

let decode_part =
  function
  | "iron_ingot" -> IronIngot
  | "copper_ingot" -> CopperIngot

  | "iron_plate" -> IronPlate
  | "iron_rod" -> IronRod
  | "wire" -> Wire
  | "cable" -> Cable
  | "concrete" -> Concrete
  | "screw" -> Screw
  | "steel_beam" -> SteelBeam
  | "steel_pipe" -> SteelPipe

  | "reinforce_iron_plate" -> ReinforcedIronPlate
  | "rotor" -> Rotor
  | "modular_frame" -> ModularFrame
  | "encased_industrial_beam" -> EncasedIndustrialBeam
  | "stator" -> Stator
  | "motor" -> Motor

  | "steel_ingot" -> SteelIngot

  | _ -> assert false

let encode_part =
  function
  | IronIngot -> "iron_ingot"
  | CopperIngot -> "copper_ingot"

  | IronPlate -> "iron_plate"
  | IronRod -> "iron_rod"
  | Wire -> "wire"
  | Cable -> "cable"
  | Concrete -> "concrete"
  | Screw -> "screw"
  | SteelBeam -> "steel_beam"
  | SteelPipe -> "steel_pipe"

  | ReinforcedIronPlate -> "reinforced_iron_plate"
  | Rotor -> "rotor"
  | ModularFrame -> "modular_frame"
  | EncasedIndustrialBeam -> "encased_industrial_beam"
  | Stator -> "stator"
  | Motor -> "motor"

  | SteelIngot -> "steel_ingot"

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

let calculation_view model =
  div []
    (List.map (fun (x, y) -> [text (encode_part x); text (string_of_int y); br [];]) model.production.input |> List.concat)

let view model =
  div []
    [
      h1 [] [ text "Factory Assistant" ];
      form_view model;
      calculation_view model;
    ]

(* MAIN *)
let main = beginnerProgram { model = (init ()); update; view }

(* EXAMPLE CONSOLE LOG *)
(* let _ =
 *   Js.log
 *     (("Hello, from BuckleScript and Reason!")[@reason.raw_literal
 *                                                "Hello, from BuckleScript and Reason!"]) *)
