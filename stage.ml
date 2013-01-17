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

let _ = 
  print_endline "stage v0.1a";
  while true do
    try
      let exps = parse $ read () in
      List.iter print_endline $ List.map Syntax.string_of_exp exps
    with _ -> print_endline "exn"
  done
