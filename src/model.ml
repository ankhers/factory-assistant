open Types

type model =
  { parts : (part * float) list
  ; production_map : production Production.t
  ; production : production
  ; total_production : (part * float) list
  ; tier : int
  }

let init () = { parts = initial_parts
              ; production_map = prod
              ; production = Production.find IronIngot prod
              ; total_production = [(IronIngot, 30.)]
              ; tier = 0
              }
