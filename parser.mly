%{
  open Syntax
%}

%token AGENT BEL PLAN
%token <Common.name> VAR
%token <string> LIT
%token <int> INT
%token LPAREN RPAREN
%token LCURLY RCURLY
%token EQUALS
%token BECOMES
%token COMMA
%token SEMICOLON
%token COLON
%token PERIOD
%token PLUS MINUS
%token SEMICOLON2
%token EOF

%token TL_CMD_DUMP
%token TL_CMD_LOAD
%token TL_CMD_QUIT

%start toplevel
%type <Syntax.exp> toplevel

%start agent_def
%type <Syntax.exp> agent_def

%left PLUS MINUS

%%

toplevel:
   | agent_def SEMICOLON2 EOF         { $1 }
   | TL_CMD_DUMP LIT                  { EToplevelCommand ("dump", [ELit $2]) }
   | TL_CMD_LOAD LIT                  { EToplevelCommand ("load", [ELit $2]) }
   | TL_CMD_QUIT                      { EToplevelCommand ("quit", []) }

agent_def:
   | AGENT VAR LCURLY agent_body RCURLY { EAgent ($2, [], $4) }
   | AGENT VAR LPAREN arg_list RPAREN LCURLY agent_body RCURLY { EAgent ($2, $4, $7) }

agent_body:
   |                                  { [] }
   | bel_def agent_body               { $1::$2 }
   | handler_def agent_body           { $1::$2 }

bel_def:
   | BEL VAR EQUALS exp SEMICOLON     { EBel ($2, $4) }

handler_def:
   | PLAN VAR LPAREN arg_list RPAREN LCURLY stmt_list RCURLY { EHandler ($2, $4, $7) }

arg_list:
   |                    { [] }
   | arg                { [$1] }
   | arg COMMA arg_list { $1 :: $3 }

arg:
   | VAR           { $1 }
   | VAR COLON VAR { $1 }

stmt_list:
   |                                  { [] }
   | stmt                             { [$1] }
   | stmt SEMICOLON stmt_list         { $1::$3 }

stmt:
   | VAR BECOMES exp                  { EUpdate ($1, $3) }

exp:
   | exp PLUS exp                     { EBinOp (BinOpPlus, $1, $3) }
   | exp MINUS exp                    { EBinOp (BinOpMinus, $1, $3) }
   | VAR                              { EVar $1 }
   | LIT                              { ELit $1 }
   | INT                              { EInt $1 }