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
%token LBRA RBRA LPAR RPAR SEMICOLON COLON COMA STAR ARROW
%token CONST FUN REC ECHO
%token IF AND OR
%token BOOL INT

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog
%type <Ast.cbool> cbool
%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  | def SEMICOLON cmds    {}
;

stat:
  ECHO expr             { ASTEcho($2) }
;

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
  | FUN REC IDENT typ LBRA args RBRA expr {ASTfunrec($3,$4,$6,$8)}
;
typ :
  BOOL { Bool }
  | INT { Int }
  | LPAR types ARROW typ RPAR {ASTfunct($2,$4)}