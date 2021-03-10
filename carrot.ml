open Printf

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


let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

type token = TToken of string | TString of string

let tokenize src =
  let rec res_to_tok dst src = match src with
    | [] -> dst
    | Str.Delim _::Text y::_::xs -> res_to_tok (dst @ [TString y]) xs
    | Text x::xs -> 
      let tokens = List.map (fun x -> TToken x) (Str.split (Str.regexp_string " ") x)
      in res_to_tok (dst @ tokens) xs
    | _ -> [] (* TODO: shouldn't reach here *) in

  src 
  |> Str.full_split (Str.regexp_string "\"")
  |> res_to_tok []
  |> let not_empty = fun x -> match x with TToken s -> String.length s > 0 | _ -> true
     in List.filter not_empty

let encode src =
  let weak_kwd t = match t with
    | "." | "->" | "=>" | "def" | "var" | "do" | ":" | ";" | "@" -> true 
    | _ -> false in

  let is_number t =
    let head = String.get t 0 in (head >= '0' && head <= '9')
  in

  let rec tok_to_code dst src = match src with
    | [] -> dst
    | TString s::xs -> tok_to_code (String s::dst) xs
    | TToken "import"::xs -> tok_to_code (Import::dst) xs
    | TToken "."::TToken x::xs -> tok_to_code (Access x::dst) xs
    | TToken "->"::TToken x::xs -> tok_to_code (Assign x::dst) xs
    | TToken "@"::TToken x::xs -> 
      let v = if (is_number x) then Int (int_of_string x) else Str x
      in tok_to_code (ArrAt v::dst) xs
    | TToken "["::xs -> tok_to_code (ArrStart::dst) xs
    | TToken "]"::xs -> tok_to_code (ArrEnd::dst) xs
    | TToken "{"::xs -> tok_to_code (ObjStart::dst) xs
    | TToken "}"::xs -> tok_to_code (ObjEnd::dst) xs
    | TToken "=>"::TToken x::xs -> tok_to_code (Ref x::dst) xs
    | TToken "("::xs -> tok_to_code (BlockStart::dst) xs
    | TToken ")"::xs -> tok_to_code (BlockEnd::dst) xs
    | TToken "def"::TToken x::TToken y::xs when y=":" || y="do" -> tok_to_code (Def x::dst) xs
    | TToken "var"::TToken x::TToken ":"::xs -> tok_to_code (Var x::dst) xs
    | TToken x::xs when x=";" || x="end" -> tok_to_code (DefVarEnd::dst) xs
    | TToken "!"::xs -> tok_to_code (Execute::dst) xs
    | TToken "?"::xs -> tok_to_code (ExecuteBranch::dst) xs
    | TToken x::xs when is_number x -> tok_to_code (Number (float_of_string x)::dst) xs
    | TToken x::xs when not (weak_kwd x) -> tok_to_code (Identifier x::dst) xs
    | _ -> [] (* TODO: shouldn't reach here *) in
    
  tok_to_code [] src


let () =
  read_whole_file "showcase.ct" 
  |> Str.global_replace (Str.regexp "#[ a-zA-Z]*") ""
  |> Str.global_replace (Str.regexp "[\n\t]") " "
  |> tokenize
  |> encode
  |> List.iter (fun x -> x |> code_to_str |> print_endline)
