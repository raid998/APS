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
  Argu(i,t) -> i
   and  eval_args a = match a with
  [] -> []
  |[b] -> [eval_arg(b)]
  |b::bs -> eval_arg b::(eval_args bs)
    
   and eval_expr e c =  match e with
      ASTNum n -> Z(n)
      (* | ASTApp(e, es) -> 
      (match e with 
        ASTId("not") -> eval_not ((eval_expr (List.hd es) c)) 
       |ASTId("add") ->  (eval_add (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("sub") ->  (eval_sub (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("mul") ->  (eval_mul (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("div") ->  (eval_div (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("eq") ->  (eval_eq (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId("lt") ->  (eval_lt (eval_expr (List.hd es) c) (eval_expr (List.hd (List.tl es)) c))
       |ASTId x -> fromSome (find_x x c)


      ) *)
    | ASTId x -> fromSome (find_x x c)
    
    |ASTif(condition,body,alternant) -> if ((eval_expr condition c )== Z(1)) then (eval_expr body c) else (eval_expr alternant c)
    |ASTfun(args,e) -> F(e,(extractArgs args),c)
    |ASTand(a,b) -> if (eval_expr a c) == Z(1) then (eval_expr b c) else Z(0)
    |ASTor(a,b) -> if (eval_expr a c) == Z(0) then (eval_expr b c) else Z(1)
    |ASTApp(e,es) -> let closure = eval_expr e c in match closure with
      |Z(_) -> failwith "Pas une fonction"
      |F(body,vars,sc) -> let vals = eval_exprs es c in eval_expr body (List.append (List.combine vars vals) sc )
      |Fr(body,name,vars,sc) -> let vals = eval_exprs es c in eval_expr body (List.append (List.append (List.combine vars vals) [(name,Fr(body,name,vars,sc))]) sc )



and eval_exprs es c=
  match es with
      [] -> []
    | [e] -> [eval_expr e c] 
    | e::es -> (eval_expr e c)::(eval_exprs es c)

and eval_stat s c =
  match s with
      ASTEcho e -> match eval_expr e c with
      | Z(n) -> string_of_int n
      | _ -> failwith "Pas affichable"


and eval_def d c= 
match d with 
    ASTconst(x,t,e) -> (x, (eval_expr e c)) :: c
    |ASTfunDef(x,t,a,e) -> (x,F(e,(eval_args a),c)) ::c
    |ASTfunRecDef(x,t,a,e) -> (x,Fr(e,x,(eval_args a),c)) ::c


(* *********************************** *)


and eval_cmd cmd c f=
  match cmd with
     ASTDef d -> (eval_def d c,f)
    |ASTStat e -> (c,eval_stat e c)


and eval_cmds cs c f=
  match cs with
    [] -> ()
    |[a] -> Printf.printf "%s" (snd (eval_cmd a c f) )
    | a::b -> eval_cmds b (List.append (fst (eval_cmd a c f)) c) f
	

(* *********************************** *)




and eval_prog p = eval_cmds p ev_env ""
;;
	
let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0
