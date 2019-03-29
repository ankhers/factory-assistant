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
    output : float;
    input : (part * float) list;
  }

let prod =
  Production.empty
    |> Production.add IronIngot { building = Smelter;
                                  output = 30.;
                                  input = [] }
    |> Production.add CopperIngot { building = Smelter;
                                    output = 30.;
                                    input = [] }

    |> Production.add IronPlate { building = Constructor;
                                  output = 15.;
                                  input = [(IronIngot, 30.)] }
    |> Production.add IronRod { building = Constructor;
                                output = 15.;
                                input = [(IronIngot, 15.)] }
    |> Production.add Wire { building = Constructor;
                             output = 45.;
                             input = [(CopperIngot, 15.)] }
    |> Production.add Cable { building = Constructor;
                              output = 15.;
                              input = [(Wire, 30.)] }
    |> Production.add Concrete { building = Constructor;
                                 output = 15.;
                                 input = [] }
    |> Production.add Screw { building = Constructor;
                              output = 90.;
                              input = [(IronRod, 15.)] }
    |> Production.add SteelBeam { building = Constructor;
                                  output = 10.;
                                  input = [(SteelIngot, 30.)] }
    |> Production.add SteelPipe { building = Constructor;
                                  output = 15.;
                                  input = [(SteelIngot, 15.)] }

    |> Production.add ReinforcedIronPlate { building = Assembler;
                                            output = 5.;
                                            input = [(IronPlate, 20.); (Screw, 120.)] }
    |> Production.add Rotor { building = Assembler;
                              output = 6.;
                              input = [(IronRod, 16.); (Screw, 132.)] }
    |> Production.add ModularFrame { building = Assembler;
                                     output = 4.;
                                     input = [(ReinforcedIronPlate, 12.); (IronRod, 24.)] }
    |> Production.add EncasedIndustrialBeam { building = Assembler;
                                              output = 4.;
                                              input = [(SteelBeam, 16.); (Concrete, 20.)] }
    |> Production.add Stator { building = Assembler;
                               output = 6.;
                               input = [(SteelPipe, 18.); (Wire, 60.)] }
    |> Production.add Motor { building = Assembler;
                              output = 5.;
                              input = [(Rotor, 10.); (Stator, 10.)] }

    |> Production.add SteelIngot { building = Foundry;
                                   output = 30.;
                                   input = [] }
