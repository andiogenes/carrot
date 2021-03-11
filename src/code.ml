type strorint = Str of string | Int of int

type code =
  | Identifier of string
  | Import
  | Access of string (* accessor identifier *)
  | Assign of string (* assignee identifier *)
  | Number of float
  | String of string
  | ArrStart
  | ArrEnd
  | ArrAt of strorint
  | ObjStart
  | ObjEnd
  | Ref of string (* reference identifier *)
  | BlockStart
  | BlockEnd
  | Def of string (* definition identifier *)
  | Var of string (* variable name *)
  | DefVarEnd
  | Execute
  | ExecuteBranch

let code_to_str c = match c with
  | Identifier x -> Printf.sprintf "Identifier(%s)" x
  | Import -> "Import"
  | Access x -> Printf.sprintf "Access(%s)" x
  | Assign x -> Printf.sprintf "Assign(%s)" x
  | Number x -> Printf.sprintf "Number(%g)" x
  | String x -> Printf.sprintf "String(\"%s\")" x
  | ArrStart -> "ArrStart"
  | ArrEnd -> "ArrEnd"
  | ArrAt (Str x) -> Printf.sprintf "At(%s)" x
  | ArrAt (Int x) -> Printf.sprintf "At(%d)" x
  | ObjStart -> "ObjStart"
  | ObjEnd -> "ObjEnd"
  | Ref x -> Printf.sprintf "Ref(%s)" x
  | BlockStart -> "BlockStart"
  | BlockEnd -> "BlockEnd"
  | Def x -> Printf.sprintf "Def(%s)" x
  | Var x -> Printf.sprintf "Var(%s)" x
  | DefVarEnd -> "DefVarEnd"
  | Execute -> "Execute"
  | ExecuteBranch -> "ExecuteBranch"