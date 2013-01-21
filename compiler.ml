open Utils
open Common
open Syntax
open Emit

module SymbolTable =
struct
  type 'a t = (name, 'a) Hashtbl.t list

  let empty = []

  let enter_scope st = (Hashtbl.create 16)::st

  let exit_scope = function
    | _::st -> st
    | _ -> failwith "empty symbol table"

  let add name value = function
    | s::_ -> Hashtbl.add s name value
    | _ -> failwith "empty symbol table"

  let rec find name = function
    | s::st ->
      (try
	 Hashtbl.find s name
       with Not_found -> find name st)
    | _ -> raise Not_found
end

type compiler_state = 
    { symbol_table: (symbol_type * index) SymbolTable.t;
      current_method: method_info option }
and symbol_type = SymField | SymArg | SymLocal

let enter_scope =
  let open StateMonad in
  perform
    cstate <-- get;
    let symtab = SymbolTable.enter_scope cstate.symbol_table in
    put { cstate with symbol_table = symtab }

let exit_scope =
  let open StateMonad in
  perform
    cstate <-- get;
    let symtab = SymbolTable.exit_scope cstate.symbol_table in
    put { cstate with symbol_table = symtab }

let in_new_scope f =
  let open StateMonad in
  perform
    enter_scope;
    r <-- f;
    exit_scope;
    return r

let register sym symty =
  let open StateMonad in
  perform
    cstate <-- get;
    let _ = SymbolTable.add sym.sym_name (symty, sym.sym_index) cstate.symbol_table in
    return ()

let register_field sym = register sym SymField
let register_param sym = register sym SymArg
let register_local sym = register sym SymLocal

let set_current_method meth =
  let open StateMonad in
  perform
    cstate <-- get;
    put { cstate with current_method = Some meth }

let in_method meth f =
  let open StateMonad in
  perform
    set_current_method meth;
    in_new_scope f

let memit opcode =
  let open StateMonad in
  perform
    cstate <-- get;
    match cstate.current_method with
    | Some mi -> let _ = emit mi opcode in return ()
    | _ -> failwith "not in a method"

let mlookup name =
  let open StateMonad in
  perform
    cstate <-- get;
    try
      return (SymbolTable.find name cstate.symbol_table)
    with Not_found -> failwith ("not in scope: " ^ name)

let rec compile_agent (EAgent (name, params, body)) =
  let open StateMonad in
  let ty = define_type name in
  in_new_scope **>
    perform
      iter (fun param ->
	register_field **> define_field param TyDescInt ty) params;
      iter (compile_member ty) body;
      return ty
and compile_member ty member = 
  let open StateMonad in
  match member with
  | EBel (name, _) ->
    register_field **> define_field name TyDescInt ty
  | EHandler (name, params, body) ->
    let meth = define_method name TyDescVoid ty in
    in_method meth **>
      perform
        let param_infos = List.map (fun param ->
	  define_parameter param TyDescInt meth) params in
	iter register_param param_infos;
        iter (compile_exp meth) body;
	memit OpRet
and compile_exp meth exp = 
  let open StateMonad in
  match exp with
  | ELocal (name, value) -> 
    perform
      register_local **> define_local name TyDescInt meth;
      compile_exp meth value;
      sym <-- mlookup name;
      memit (OpStloc (snd sym))
  | EUpdate (name, value) -> 
    perform
      compile_exp meth value;
      sym <-- mlookup name;
      memit (OpStfld (snd sym))
  | EBinOp (op, left, right) -> 
    perform
      compile_exp meth left;
      compile_exp meth right;
      memit OpAdd
  | ERef name ->
    perform
      sym <-- mlookup name;
      (match fst sym with
      | SymField -> memit (OpLdfld (snd sym))
      | SymArg -> memit (OpLdarg (snd sym))
      | SymLocal -> memit (OpLdloc (snd sym)))
  | ELit lit -> memit (OpLdStr lit)
  | EInt num -> memit (OpLdcI4 num)

let compile exp =
  let cstate = { symbol_table = SymbolTable.empty;
		 current_method = None } in
  StateMonad.run (compile_agent exp) cstate
