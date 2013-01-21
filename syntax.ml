open Utils
open Common

type agent_desc = EAgent of name * name list * member_desc list
and member_desc = 
    EBel of name * exp
  | EHandler of name * name list * exp list
and exp =
    ELocal of name * exp
  | EUpdate of name * exp
  | EBinOp of binop * exp * exp
  | ERef of name
  | ELit of string
  | EInt of int
and binop = BinOpPlus | BinOpMinus

and toplevel =
    TLAgent of agent_desc
  | TLCommand of string * exp list

let string_of_seq f sep seq =
  String.implode sep (List.map f seq)

let rec string_of_agent_desc (EAgent (name, args, body)) =
  "agent " ^ name ^ " (" ^ (String.implode ", " args) ^ ") { " ^ (string_of_seq string_of_member_desc " " body) ^ " }"
and string_of_member_desc = function
  | EBel (name, arg) ->
    "bel " ^ name ^ " = " ^ (string_of_exp arg) ^ ";"
  | EHandler (name, args, (body: exp list)) ->
    "plan " ^ name ^ "(" ^ (String.implode ", " args) ^ ") { " ^ (string_of_seq string_of_exp "; " body) ^ " }"
and string_of_exp : exp -> string = function
  | ELocal (name, value) -> "let " ^ name ^ " = " ^ (string_of_exp value)
  | EUpdate (name, arg) -> name ^ " := " ^ (string_of_exp arg)
  | EBinOp (BinOpPlus, e1, e2) -> (string_of_exp e1) ^ " + " ^ (string_of_exp e2)
  | EBinOp (BinOpMinus, e1, e2) -> (string_of_exp e1) ^ " - " ^ (string_of_exp e2)
  | ERef name -> name
  | ELit l -> "\"" ^ l ^ "\""
  | EInt i -> string_of_int i

