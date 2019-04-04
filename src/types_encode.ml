open Types

let encode_part =
  function
  | IronIngot -> "Iron Ingot"
  | CopperIngot -> "Copper Ingot"

  | IronPlate -> "Iron Plate"
  | IronRod -> "Iron Rod"
  | Wire -> "Wire"
  | Cable -> "Cable"
  | Concrete -> "Concrete"
  | Screw -> "Screw"
  | SteelBeam -> "Steel Beam"
  | SteelPipe -> "Steel Pipe"

  | ReinforcedIronPlate -> "Reinforced Iron Plate"
  | Rotor -> "Rotor"
  | ModularFrame -> "Modular Frame"
  | EncasedIndustrialBeam -> "Encased Industrial Beam"
  | Stator -> "Stator"
  | Motor -> "Motor"

  | SteelIngot -> "Steel Ingot"

let encode_building =
  function
  | Smelter -> "Smelter"
  | Constructor -> "Constructor"
  | Assembler -> "Assembler"
  | Foundry -> "Foundry"

let encode_logistic =
  function
  | Splitter n -> "Splitter (" ^ string_of_int n ^ ")"
  | Merger n -> "Merger (" ^ string_of_int n ^ ")"
  | Nothing -> "Nothing"
