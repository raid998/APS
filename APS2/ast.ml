(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typ = 
    SType of sType
  | FuncT of typ list

and sType =
    Int
  | Bool
  | Void
  | Vec of sType


type arg = 
    Argu of string*typ

type argp = 
    Argp of string*typ
  | Argpv of string*typ
  
type expr =
    ASTNum of int
  | ASTId of string
  | ASTif of expr*expr*expr
  | ASTand of expr*expr
  | ASTor of expr*expr
  | ASTApp of expr * expr list
  | ASTfun of arg list*expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNthE of expr*expr
  | ASTVset of expr*expr*expr

type exprp = 
    ASTExpr of expr
  | ASTAdr of string

type stat =
      ASTEcho of expr
    | ASTSet of lval * expr
    | ASTIff of expr * cmd list*cmd list
    | ASTloop of expr * cmd list
    | ASTCall of string * exprp list
    
and def = 
    ASTconst of string*typ*expr
  | ASTfunDef of string*typ*arg list*expr
  | ASTfunRecDef of string*typ*arg list*expr
  | ASTVar of string * sType
  | ASTProc of string * argp list * cmd list
  | ASTProcRec of string * argp list * cmd list

      
and cmd =
     ASTStat of stat
    |ASTDef of def

and lval = 
    ASTLval of string
  | ASTNthL of lval*expr
	
