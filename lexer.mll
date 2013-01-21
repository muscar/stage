{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+
let lit = '"' [^ '\r' '\n' '"']* '"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| ['0'-'9']+           { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "agent"              { AGENT }
| "bel"                { BEL }
| "plan"               { PLAN }
| "let"                { LET }
| "("                  { LPAREN }
| ")"                  { RPAREN }
| "{"                  { LCURLY }
| "}"                  { RCURLY }
| ":="                 { BECOMES }
| "="                  { EQUALS }
| ","                  { COMMA }
| ";"                  { SEMICOLON }
| ":"                  { COLON }
| ";;"                 { SEMICOLON2 }
| "."                  { PERIOD }
| "+"                  { PLUS }
| "-"                  { MINUS }

| "#dump"              { TL_CMD_DUMP }
| "#load"              { TL_CMD_LOAD }
| "#quit"              { TL_CMD_QUIT }

| lit                  { let lexeme = Lexing.lexeme lexbuf in LIT (String.sub lexeme 1 (String.length lexeme - 2)) }
| var                  { VAR (Lexing.lexeme lexbuf) }
| eof                  { EOF }
