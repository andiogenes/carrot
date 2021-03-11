let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

type token = Token of string | String of string

exception Should_not_reach_here

let tokenize src =
  let rec res_to_tok dst src = match src with
    | [] -> dst
    | Str.Delim _::Text y::_::xs -> res_to_tok (dst @ [String y]) xs
    | Text x::xs -> 
      let tokens = List.map (fun x -> Token x) (Str.split (Str.regexp_string " ") x)
      in res_to_tok (dst @ tokens) xs
    | _ -> raise Should_not_reach_here in

  src 
  |> Str.full_split (Str.regexp_string "\"")
  |> res_to_tok []
  |> let not_empty = fun x -> match x with Token s -> String.length s > 0 | _ -> true
     in List.filter not_empty

let encode src =
  let weak_kwd t = match t with
    | "." | "->" | "=>" | "def" | "var" | "do" | ":" | ";" | "@" -> true 
    | _ -> false in

  let is_number t =
    let head = String.get t 0 in (head >= '0' && head <= '9')
  in

  let rec tok_to_code dst src = let module C = Code in match src with
    | [] -> List.rev dst
    | String s::xs -> tok_to_code (C.String s::dst) xs
    | Token "import"::xs -> tok_to_code (Import::dst) xs
    | Token "."::Token x::xs -> tok_to_code (Access x::dst) xs
    | Token "->"::Token x::xs -> tok_to_code (Assign x::dst) xs
    | Token "@"::Token x::xs -> 
      let v = if (is_number x) then C.Int (int_of_string x) else Str x
      in tok_to_code (ArrAt v::dst) xs
    | Token "["::xs -> tok_to_code (ArrStart::dst) xs
    | Token "]"::xs -> tok_to_code (ArrEnd::dst) xs
    | Token "{"::xs -> tok_to_code (ObjStart::dst) xs
    | Token "}"::xs -> tok_to_code (ObjEnd::dst) xs
    | Token "=>"::Token x::xs -> tok_to_code (Ref x::dst) xs
    | Token "("::xs -> tok_to_code (BlockStart::dst) xs
    | Token ")"::xs -> tok_to_code (BlockEnd::dst) xs
    | Token "def"::Token x::Token y::xs when y=":" || y="do" -> tok_to_code (Def x::dst) xs
    | Token "var"::Token x::Token ":"::xs -> tok_to_code (Var x::dst) xs
    | Token x::xs when x=";" || x="end" -> tok_to_code (DefVarEnd::dst) xs
    | Token "!"::xs -> tok_to_code (Execute::dst) xs
    | Token "?"::xs -> tok_to_code (ExecuteBranch::dst) xs
    | Token x::xs when is_number x -> tok_to_code (Number (float_of_string x)::dst) xs
    | Token x::xs when not (weak_kwd x) -> tok_to_code (Identifier x::dst) xs
    | _ -> raise Should_not_reach_here in
    
  tok_to_code [] src

let pass_file filename = 
  read_whole_file filename 
  |> Str.global_replace (Str.regexp "#[ a-zA-Z]*") ""
  |> Str.global_replace (Str.regexp "[\n\t]") " "
  |> tokenize
  |> encode