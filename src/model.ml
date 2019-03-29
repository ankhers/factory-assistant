open Types

type model =
  { part : part;
    number : int;
    production_map : production Production.t;
    production : production;
  }
