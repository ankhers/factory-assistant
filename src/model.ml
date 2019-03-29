open Types

type model =
  { part : part;
    number : float;
    production_map : production Production.t;
    production : production;
  }
