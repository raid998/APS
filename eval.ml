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

let (ev_env:(string*v) list) = [("true",Z(1));("false",Z(0))];;


(* ------------------------------------------------ *)

let rec find_x x e = 
  match e with 
   [] -> None
  | (a,v)::rest -> if a == x then Some v else find_x x rest

(* ------------------------------------------------ *)

let fromSome x = match x with 
  Some v -> v

(* ------------------------------------------------ *)

let eval_not x = match x with
  Z(0) -> Z(1)
  |Z(1) -> Z(0)

let eval_add x y = match x,y with
Z(x1),Z(y1) -> Z(x1+y1)

let eval_sub x y = match x,y with
Z(x1),Z(y1) -> Z(x1-y1)

let eval_mul x y = match x,y with
Z(x1),Z(y1) -> Z(x1*y1)

let eval_div x y = match x,y with
Z(x1),Z(y1) -> Z(x1/y1)

let eval_eq x y = match x,y with
Z(x1),Z(y1) -> if(x1==y1) then Z(1) else Z(0)

let eval_lt x y = match x,y with
Z(x1),Z(y1) -> if(x1<y1) then Z(1) else Z(0)

let rec extractArgs l= match l with
  [] -> []
  |Argu(a,t)::xs -> a::(extractArgs xs)
  

(* ------------------------------------------------ *)
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
    
  ) and eval_expr e c =  match e with
      ASTNum n -> Z(n)
    | ASTId x -> fromSome (find_x x c)
    | ASTApp(e, es) -> 
      (match e with 
        ASTId("not") -> eval_not ((eval_expr (List.hd es) c)) 
       |ASTId("add") ->  (eval_add (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("sub") ->  (eval_sub (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("mul") ->  (eval_mul (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("div") ->  (eval_div (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("eq") ->  (eval_eq (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("lt") ->  (eval_lt (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))



      )
    |ASTif(condition,body,alternant) -> if ((eval_expr condition c )== Z(1)) then (eval_expr body c) else (eval_expr alternant c)
    |ASTfun(args,e) -> F(e,(extractArgs args),c)
    |ASTand(a,b) -> if (eval_expr a c) == Z(1) then (eval_expr b c) else Z(0)
    |ASTor(a,b) -> if (eval_expr a c) == Z(0) then (eval_expr b c) else Z(1)
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
      
