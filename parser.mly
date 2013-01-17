%{
  open Syntax
%}

%token AGENT BEL PLAN
%token <Syntax.name> VAR
%token <int> INT
%token LPAREN RPAREN
%token LCURLY RCURLY
%token QMARK
%token EQUALS
%token COMMA
%token SEMICOLON
%token COLON
%token PERIOD
%token PLUS MINUS
%token SEMICOLON2
%token EOF

%start toplevel
%type <Syntax.exp list> toplevel

%left PLUS MINUS

%%

toplevel:
   | EOF                              { [] }
   | agent_def SEMICOLON2 EOF         { [$1] }

agent_def:
   | AGENT VAR LCURLY agent_body RCURLY { EAgent ($2, $4) }

agent_body:
   |                                  { [] }
   | bel_def agent_body               { $1::$2 }
   | handler_def agent_body           { $1::$2 }

bel_def:
   | BEL VAR EQUALS exp PERIOD        { EBel ($2, $4) }

handler_def:
   | PLAN VAR LPAREN arg_list RPAREN EQUALS stmt_list PERIOD { EHandler ($2, $4, $7) }

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
   | QMARK VAR LPAREN id_list RPAREN  { EQuery ($2, $4) }
   | PLUS VAR LPAREN exp_list RPAREN  { EUpdate ($2, $4) }

exp_list:
   |                                  { [] }
   | exp                              { [$1] }
   | exp COMMA exp_list               { $1::$3 }

exp:
   | exp PLUS exp                     { EBinOp (BinOpPlus, $1, $3) }
   | exp MINUS exp                    { EBinOp (BinOpMinus, $1, $3) }
   | VAR                              { EVar $1 }
   | INT                              { EInt $1 }

id_list:
   |                                  { [] }
   | VAR                              { [$1] }
   | VAR COMMA id_list                { $1::$3 }
