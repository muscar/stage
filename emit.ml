open Utils
open Common

(* module ConstantPool : sig *)
(*   type t *)
    
(*   val create : unit -> t *)
(*   val intern : t -> name -> int *)
(* end = struct *)
(*   type t = (name, int) Hashtbl.t * int ref *)
      
(*   let create () = (Hashtbl.create 1024, ref 0) *)
    
(*   let intern (pool, idx) name =  *)
(*     try *)
(*       Hashtbl.find pool name *)
(*     with Not_found -> *)
(*       let index = !idx in  *)
(*       Hashtbl.add pool name index; *)
(*       idx := index + 1; *)
(*       index *)
(* end *)

type opcode =
    OpLdfld of index
  | OpStfld of index
  | OpLdloc of index
  | OpStloc of index
  | OpLdarg of index
  | OpLdStr of string
  | OpLdcI4 of int
  | OpAdd
  | OpRet

type type_desc =
    TyDescVoid
  | TyDescInt
  | TyDescStr
  | TyDescObj of string

type 'a symbol_info =
    { sym_index: index;
      sym_type: type_desc;
      sym_name: string }

type sym_field
type sym_param
type sym_local

type field_info = sym_field symbol_info

type param_info = sym_param symbol_info

type local_info = sym_local symbol_info

type method_info =
    { method_declaring_type: type_info;
      method_ret_type: type_desc;
      method_params: param_info DynArray.t;
      method_name: string;
      method_body: method_body }
and method_body =
    { method_bytecode: opcode DynArray.t;
      method_locals: local_info DynArray.t }

and type_info =
    { type_name: string ;
      type_fields: field_info DynArray.t;
      type_methods: method_info DynArray.t }

let define_type name =
  { type_name = name;
    type_fields = DynArray.make 128;
    type_methods = DynArray.make 128 }

let define_field name ty_desc ty : field_info =
  let field_info = { sym_index = DynArray.length ty.type_fields;
		     sym_type = ty_desc;
		     sym_name = name } in
  DynArray.add ty.type_fields field_info;
  field_info

let define_method name ret_ty_desc ty =
  let method_body = { method_bytecode = DynArray.make 128;
		      method_locals = DynArray.make 32 } in
  let method_info = { method_declaring_type = ty;
		      method_ret_type = ret_ty_desc;
		      method_params = DynArray.create ();
		      method_name = name;
		      method_body = method_body } in
  DynArray.add ty.type_methods method_info;
  method_info

let define_parameter name ty_desc meth : param_info =
  let param_info = { sym_index = DynArray.length meth.method_params;
		     sym_type = ty_desc;
		     sym_name = name } in
  DynArray.add meth.method_params param_info;
  param_info

let define_local name ty_desc meth : local_info =
  let local_info = { sym_index = DynArray.length meth.method_body.method_locals;
		     sym_type = ty_desc;
		     sym_name = name } in
  DynArray.add meth.method_body.method_locals local_info;
  local_info

let emit meth opcode = 
  DynArray.add meth.method_body.method_bytecode opcode

(* Accessors, mostly for debugging *)

let get_sym_at idx arr = DynArray.get arr idx

let get_sym name arr = 
  DynArray.get arr **> DynArray.index_of (fun sym ->
    sym.sym_name = name) arr

let get_method name ty = 
  DynArray.get ty.type_methods **> DynArray.index_of (fun meth ->
    meth.method_name = name) ty.type_methods

let get_bytecode meth =
  DynArray.to_list meth.method_body.method_bytecode
