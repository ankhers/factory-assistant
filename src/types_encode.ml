open Types

let encode_part =
  function
  | IronIngot -> "Iron Ingot"
  | CopperIngot -> "Copper Ingot"

  | SteelIngot -> "Steel Ingot"

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
  | CircuitBoard -> "Circuit Board"

  | Plastic -> "Plastic"
  | Rubber -> "Rubber"

  | HeavyModularFrame -> "Heavy Modular Frame"
  | Computer -> "Computer"

let encode_building =
  function
  | Smelter -> "Smelter"
  | Constructor -> "Constructor"
  | Assembler -> "Assembler"
  | Foundry -> "Foundry"
  | OilRefinery -> "Oil Refinery"
  | Manufacturer -> "Manufacturer"

let encode_logistic =
  function
  | Splitter _ -> "Splitter"
  | Merger _ -> "Merger"
