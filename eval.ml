(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast


type v = 
    | Z of int
    | F of expr*string list*(string*v) list
    | Fr of expr*string*string list*(string*v) list

let (ev_env:(string*v) list) = [];;


let rec eval_arg a = match a with 
  Argu(i,t) -> (
  ) and  eval_args a = match a with
  [] -> ()
  |[b] -> ();
  |b -> (
    let rec eval_args_aux l = match l with
      [] -> ()
      |[le] -> ()
      | lh::lt -> ()
      in
     ()
    
  ) and eval_expr e c=
  match e with
      ASTNum n -> Z(int_of_string n)
    | ASTId x -> ()
    | ASTApp(e, es) -> ()
    | ASTif(condition,body,alternant) -> ()
    |ASTfun(args,e) -> ()
    |ASTand(a,b) -> ()
    |ASTor(a,b) -> ()
and eval_exprs es =
  match es with
      [] -> ()
    | [e] -> ()
    | e::es -> ()

and eval_stat s =
  match s with
      ASTEcho e -> ()

and eval_type t = 
  match t with 
    Bool -> ()
    |Int -> ()
    |FuncT(ts) -> ()

and eval_def d = 
match d with 
    ASTconst(i,t,e) -> ()
    |ASTfunDef(i,t,a,e) -> ()
    |ASTfunRecDef(i,t,a,e) -> ()
and eval_cmd c =
  match c with
      ASTStat s -> ()
     |ASTDef d -> ()



and eval_cmds cs =
  match cs with
    [] -> ()
    |c::[] -> ()
    | a::b -> ()
	
and eval_prog p = ()
;;
	
(*let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0 *)
      
