open Utils
open Common
open Syntax
open Bytecode

module ConstantPool : sig
  type t
    
  val create : unit -> t
  val intern : t -> name -> int32
end = struct
  type t = (name, int32) Hashtbl.t * int32 ref
      
  let create () = (Hashtbl.create 1024, ref 0l)
    
  let intern (pool, idx) name = 
    try
      Hashtbl.find pool name
    with Not_found ->
      let index = !idx in 
      Hashtbl.add pool name index;
      idx := Int32.succ index;
      index
end

type agent_file =
    { constant_pool: int32 }

let compile =
  let pool = ConstantPool.create () in
  function
  | EAgent (name, args, body) ->
    ConstantPool.intern pool name
  | _ -> failwith "can't compile"

