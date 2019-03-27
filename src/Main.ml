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

(* MODEL *)
type model =
  { part : part;
    number : int
  }
let init () = { part = IronIngot; number = 5 }

(* UPDATE *)
type msg =
  | SetPart of part
  | ChangeNumber of int

let update model =
  function
  | SetPart part -> { model with part }
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

let view model =
  div []
    [
      h1 [] [ text "Factory Assistant" ];
      select
        [
          onChange (fun part -> SetPart (decode_part part));
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
      input'
        [
          onInput (fun s -> ChangeNumber (int_of_string s));
          type' "number";
          value (string_of_int model.number)
        ] [];
    ]

(* MAIN *)
let main = beginnerProgram { model = (init ()); update; view }

(* EXAMPLE CONSOLE LOG *)
let _ =
  Js.log
    (("Hello, from BuckleScript and Reason!")[@reason.raw_literal
                                               "Hello, from BuckleScript and Reason!"])
