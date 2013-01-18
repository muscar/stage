open Utils
open Common

type exp =
    EAgent of name * name list * exp list
  | EBel of name * exp
  | EHandler of name * name list * exp list
  | EUpdate of name * exp
  | EBinOp of binop * exp * exp
  | EVar of name
  | ELit of string
  | EInt of int

  | EToplevelCommand of string * exp list
and binop = BinOpPlus | BinOpMinus

let rec string_of_exp = function
  | EAgent (name, args, body) ->
    "agent " ^ name ^ " (" ^ (String.implode ", " args) ^ ") { " ^ (string_of_seq " " body) ^ " }"
  | EBel (name, arg) ->
    "bel " ^ name ^ " = " ^ (string_of_exp arg) ^ ";"
  | EHandler (name, args, body) ->
    "plan " ^ name ^ "(" ^ (String.implode ", " args) ^ ") { " ^ (string_of_seq "; " body) ^ " }"
  | EUpdate (name, arg) -> name ^ " := " ^ (string_of_exp arg)
  | EBinOp (BinOpPlus, e1, e2) -> (string_of_exp e1) ^ " + " ^ (string_of_exp e2)
  | EBinOp (BinOpMinus, e1, e2) -> (string_of_exp e1) ^ " - " ^ (string_of_exp e2)
  | EVar name -> name
  | ELit l -> "\"" ^ l ^ "\""
  | EInt i -> string_of_int i
  | EToplevelCommand _ -> "<cmd>"
and string_of_seq sep seq =
  String.implode sep (List.map string_of_exp seq)
