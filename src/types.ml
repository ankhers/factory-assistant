type logitistics =
  | Splitter
  | Merger
  | Nothing

type production_buildings =
  | Smelter
  | Constructor
  | Assembler
  | Foundry

type production_building =
  { building : production_buildings;
    power_consumption : float;
  }

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
    output : float;
    input : (part * float) list;
  }

let smelter = { building = Smelter; power_consumption = 4.}

let constructor = { building = Constructor; power_consumption = 4.}

let assembler = { building = Assembler; power_consumption = 15.}

let foundry = { building = Foundry; power_consumption = 16.}

let prod =
  Production.empty
    |> Production.add IronIngot { building = smelter;
                                  output = 30.;
                                  input = [] }
    |> Production.add CopperIngot { building = smelter;
                                    output = 30.;
                                    input = [] }

    |> Production.add IronPlate { building = constructor;
                                  output = 15.;
                                  input = [(IronIngot, 30.)] }
    |> Production.add IronRod { building = constructor;
                                output = 15.;
                                input = [(IronIngot, 15.)] }
    |> Production.add Wire { building = constructor;
                             output = 45.;
                             input = [(CopperIngot, 15.)] }
    |> Production.add Cable { building = constructor;
                              output = 15.;
                              input = [(Wire, 30.)] }
    |> Production.add Concrete { building = constructor;
                                 output = 15.;
                                 input = [] }
    |> Production.add Screw { building = constructor;
                              output = 90.;
                              input = [(IronRod, 15.)] }
    |> Production.add SteelBeam { building = constructor;
                                  output = 10.;
                                  input = [(SteelIngot, 30.)] }
    |> Production.add SteelPipe { building = constructor;
                                  output = 15.;
                                  input = [(SteelIngot, 15.)] }

    |> Production.add ReinforcedIronPlate { building = assembler;
                                            output = 5.;
                                            input = [(IronPlate, 20.); (Screw, 120.)] }
    |> Production.add Rotor { building = assembler;
                              output = 6.;
                              input = [(IronRod, 16.); (Screw, 132.)] }
    |> Production.add ModularFrame { building = assembler;
                                     output = 4.;
                                     input = [(ReinforcedIronPlate, 12.); (IronRod, 24.)] }
    |> Production.add EncasedIndustrialBeam { building = assembler;
                                              output = 4.;
                                              input = [(SteelBeam, 16.); (Concrete, 20.)] }
    |> Production.add Stator { building = assembler;
                               output = 6.;
                               input = [(SteelPipe, 18.); (Wire, 60.)] }
    |> Production.add Motor { building = assembler;
                              output = 5.;
                              input = [(Rotor, 10.); (Stator, 10.)] }

    |> Production.add SteelIngot { building = foundry;
                                   output = 30.;
                                   input = [] }

let initial_parts = [(IronIngot, 30.)]
