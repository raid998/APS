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
  Bool
  | Int
  | FuncT of typ list




  type arg = 
  Argu of string*typ

  
type expr =
    ASTNum of int
  | ASTId of string
  | ASTif of expr*expr*expr
  | ASTand of expr*expr
  | ASTor of expr*expr
  | ASTApp of expr * expr list
  | ASTfun of arg list*expr

type stat =
    ASTEcho of expr
    
type def = 
 ASTconst of string*typ*expr
  | ASTfunDef of string*typ*arg list*expr
  | ASTfunRecDef of string*typ*arg list*expr

      
type cmd =
    ASTStat of stat
    |ASTDef of def


	
