open Types

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

let encode_building =
  function
  | Smelter -> "Smelter"
  | Constructor -> "Constructor"
  | Assembler -> "Assembler"
  | Foundry -> "Foundry"
