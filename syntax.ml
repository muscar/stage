open Utils

type name = string

type exp =
    EAgent of name * exp list
  | EBel of name * exp
  | EHandler of name * name list * exp list
  | EQuery of name * name list
  | EUpdate of name * exp list
  | EBinOp of binop * exp * exp
  | EVar of name
  | EInt of int
and binop = BinOpPlus | BinOpMinus

let rec string_of_exp = function
  | EAgent (name, body) ->
    "agent " ^ name ^ " { " ^ (string_of_seq " " body) ^ " }"
  | EBel (name, arg) ->
    "bel " ^ name ^ " = " ^ (string_of_exp arg) ^ "."
  | EHandler (name, args, body) ->
    name ^ "(" ^ (String.implode ", " args) ^ ") = " ^ (string_of_seq "; " body) ^ "."
  | EQuery (name, args) ->
    "?" ^ name ^ "(" ^ (String.implode ", " args) ^ ")"
  | EUpdate (name, args) ->
    "+" ^ name ^ "(" ^ (string_of_seq ", " args) ^ ")"
  | EBinOp (BinOpPlus, e1, e2) -> (string_of_exp e1) ^ " + " ^ (string_of_exp e2)
  | EBinOp (BinOpMinus, e1, e2) -> (string_of_exp e1) ^ " - " ^ (string_of_exp e2)
  | EVar name -> name
  | EInt i -> string_of_int i
and string_of_seq sep seq =
  String.implode sep $ List.map string_of_exp seq
