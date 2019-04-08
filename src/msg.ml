open Types

type msg =
  | ChangePart of int * part
  | ChangeQuantity of int * float
  | AddPart
  | RemovePart of int

  | ChangeTier of int
