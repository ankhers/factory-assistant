open Types

type model =
  { parts : (part * float) list;
    production_map : production Production.t;
    production : production;
    total_production : (part * float) list
  }
