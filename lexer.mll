{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| ['0'-'9']+           { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "agent"              { AGENT }
| "bel"                { BEL }
| "("                  { LPAREN }
| ")"                  { RPAREN }
| "{"                  { LCURLY }
| "}"                  { RCURLY }
| "?"                  { QMARK }
| "="                  { EQUALS }
| ","                  { COMMA }
| ";"                  { SEMICOLON }
| ";;"                 { SEMICOLON2 }
| "."                  { PERIOD }
| "+"                  { PLUS }
| "-"                  { MINUS }
| var                  { VAR (Lexing.lexeme lexbuf) }
| eof                  { EOF }
