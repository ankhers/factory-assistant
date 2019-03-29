open Types

let decode_part =
  function
  | "Iron Ingot" -> IronIngot
  | "Copper Ingot" -> CopperIngot

  | "Iron Plate" -> IronPlate
  | "Iron Rod" -> IronRod
  | "Wire" -> Wire
  | "Cable" -> Cable
  | "Concrete" -> Concrete
  | "Screw" -> Screw
  | "Steel Beam" -> SteelBeam
  | "Steel Pipe" -> SteelPipe

  | "Reinforced Iron Plate" -> ReinforcedIronPlate
  | "Rotor" -> Rotor
  | "Modular Frame" -> ModularFrame
  | "Encased Industrial Beam" -> EncasedIndustrialBeam
  | "Stator" -> Stator
  | "Motor" -> Motor

  | "Steel Ingot" -> SteelIngot

  | _ -> assert false
