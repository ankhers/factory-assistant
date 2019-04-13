open Types

let decode_part =
  function
  | "Iron Ingot" -> IronIngot
  | "Copper Ingot" -> CopperIngot

  | "Steel Ingot" -> SteelIngot

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
  | "Circuit Board" -> CircuitBoard

  | "Plastic" -> Plastic
  | "Rubber" -> Rubber

  | "Heavy Modular Frame" -> HeavyModularFrame
  | "Computer" -> Computer

  | _ -> assert false
