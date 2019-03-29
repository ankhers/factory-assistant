open Types

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

  | "reinforced_iron_plate" -> ReinforcedIronPlate
  | "rotor" -> Rotor
  | "modular_frame" -> ModularFrame
  | "encased_industrial_beam" -> EncasedIndustrialBeam
  | "stator" -> Stator
  | "motor" -> Motor

  | "steel_ingot" -> SteelIngot

  | _ -> assert false
