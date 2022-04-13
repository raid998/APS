%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LBRA RBRA LPAR RPAR SEMICOLON COLON COMMA STAR ARROW
%token CONST VAR PROC FUN REC ECHO
%token IF IFF WHILE AND OR
%token BOOL INT VOID
%token SET CALL 

%type <Ast.expr> expr
%type <Ast.typ> typ
%type <Ast.arg> arg
%type <Ast.arg list> args 
%type <Ast.def> def
%type <Ast.typ list> types
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog
%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

block : LBRA cmds RBRA { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  | def SEMICOLON cmds    {ASTDef($1)::$3}
  | stat SEMICOLON cmds {ASTStat($1)::$3}
;

stat:
  ECHO expr             { ASTEcho($2) }
  | SET IDENT expr        { ASTSet($2,$3)}
  | IFF expr block block {ASTIff($2,$3,$4)}
  | WHILE expr block {ASTloop($2,$3)}
  | CALL IDENT exprs { ASTCall($2,$3)}
;

arg: 
  IDENT COLON typ {Argu($1, $3)}
;

args: 
  arg {[$1]}
  |arg COMMA args {$1::$3}
expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR {ASTif($3,$4,$5)}
| LPAR AND expr expr RPAR {ASTand($3,$4)}
| LPAR OR expr expr RPAR {ASTor($3,$4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr { ASTfun($2,$4) }

;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;
def : 
  CONST IDENT typ expr  { ASTconst($2,$3,$4) }
  | FUN IDENT typ LBRA args RBRA expr {ASTfunDef($2,$3,$5,$7)}
  | FUN REC IDENT typ LBRA args RBRA expr {ASTfunRecDef($3,$4,$6,$8)}
  | VAR IDENT typ {ASTVar($2,$3)}
  | PROC IDENT LBRA args RBRA block {ASTProc($2,$4,$6)}
  | PROC REC IDENT LBRA args RBRA block {ASTProcRec($3,$5,$7)}
  
;
typ :
  BOOL { Bool }
  | INT { Int }
  | VOID { Void }
  | LPAR types ARROW typ RPAR {FuncT($2 @ [$4])}
;
types: 
 typ {[$1]}
 | typ STAR types {$1 :: $3}