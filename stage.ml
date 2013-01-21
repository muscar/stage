open Utils
open Syntax
open Lexer
open Parser

let prompt p =
  print_string p;
  read_line ()

let read =
  let buf = Buffer.create 256 in
  let rec loop line =
    Buffer.add_string buf line;
    if String.endswith line ";;" then Buffer.contents buf
    else loop (prompt "= ") in
  fun () ->
    Buffer.clear buf;
    loop (prompt "- ")

let parse =
  Parser.toplevel Lexer.token >> Lexing.from_string

let load_file path =
  let chan = open_in path in
  let exp = Parser.agent_def Lexer.token (Lexing.from_channel chan) in
  close_in chan;
  exp

let dump_file path contents =
  let chan = open_out path in
  output_string chan contents;
  close_out chan

let rec toplevel () =
  let history = Buffer.create 1024 in
  let do_exit = ref false in
  while not !do_exit do
    try
      let line = read () in
      match parse line with
      | TLCommand ("dump", [Syntax.ELit path]) ->
	dump_file path (Buffer.contents history)
      | TLCommand ("load", [Syntax.ELit path]) ->
	print_endline (Syntax.string_of_agent_desc (load_file path))
      | TLCommand ("quit", []) -> raise End_of_file
      | TLCommand (cmd, _) -> failwith ("unknown command: " ^ cmd)
      | TLAgent exp -> 
	Buffer.add_string history line;
	Compiler.compile exp |> ignore;
	print_endline (Syntax.string_of_agent_desc exp)
    with
    | End_of_file -> 
      print_endline "Bye!";
      do_exit := true
    | _ -> print_endline "exn"
  done

let _ = 
  print_endline "stage v0.1a";
  toplevel ()
