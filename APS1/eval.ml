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

let type_of e = match e with

  | ASTStat _ -> "ASTStat"
  |ASTDef _ -> "ASTDef"

type v = 
    | Z of int
    | F of expr*string list*(string*v) list
    | Fr of expr*string*string list*(string*v) list
    | A of int ref
let (ev_env:(string*v) list) = [("true",Z(1));("false",Z(0))];;

let mem_counter = ref 0;;

let (mem_env: (int ref*v option) list) = [];;

let alloc mem = let res =  (!mem_counter, ((!mem_counter, None)::mem)) in mem_counter := (!mem_counter + 1) ; res;;

let rec find_from_mem a mem =
  match mem with 
  [] -> failwith "address not in memory"
  |(x,v)::rest -> if x = a then (
    match v with 
    None -> failwith "undefined"
    |Some v1 -> v1
  ) else find_from_mem a rest
(* ------------------------------------------------ *)

let rec find_id x e m = 
  match e with 
   [] -> failwith x
  | (a,v)::rest -> if (String.equal a x) then (
    match v with 
    A(i) -> find_from_mem i m
    |_ -> v
    ) else find_id x rest m

(* ------------------------------------------------ *)


let print_z x = match x with
  Z(n) -> string_of_int n

let idGetter x = match x with 
  ASTId(a) -> ("ASTId" ^ a)
  |ASTApp(_,_) -> ("ASTApp")


 let print_bool x = match x with 
 true -> "true"
 |false -> "false" 

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
    
   and eval_expr e c m=  match e with
      ASTNum n -> Z(n)    
      | ASTApp(e1, es) -> 
      (match e1 with 
        ASTId("not") -> eval_not ((eval_expr (List.hd es) c m)) 
       |ASTId("add") ->  (eval_add (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       |ASTId("sub") ->  (eval_sub (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       |ASTId("mul") ->  (eval_mul (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       |ASTId("div") ->  (eval_div (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       |ASTId("eq") ->  (eval_eq (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       |ASTId("lt") ->  (eval_lt (eval_expr (List.hd es) c m) (eval_expr (List.hd (List.tl es)) c m))
       | _ -> let closure = eval_expr e1 c m in (match closure with
      |Z(_) -> failwith "Pas une fonction"
      |F(body,vars,sc) -> let vals = eval_exprs es c m in eval_expr body (List.append (List.combine vars vals) sc ) m
      |Fr(body,name,vars,sc) -> let vals = eval_exprs es c m in eval_expr body (List.append (List.append (List.combine vars vals) [(name,Fr(body,name,vars,sc))]) sc ) m)
      )
    
    | ASTId(x) -> (find_id x c m)
    
    |ASTif(condition,body,alternant) -> if ((eval_expr condition c m) = Z(1)) then (eval_expr body c m) else (eval_expr alternant c m)
    |ASTfun(args,e1) -> F(e1,(extractArgs args),c)
    |ASTand(a,b) -> if (eval_expr a c m) = Z(1) then (eval_expr b c m) else Z(0)
    |ASTor(a,b) -> if (eval_expr a c m) = Z(0) then (eval_expr b c m) else Z(1)
    



and eval_exprs es c m =
  match es with
      [] -> []
    | [e] -> [eval_expr e c m] 
    | e::es -> (eval_expr e c m)::(eval_exprs es c m)

and eval_stat s c m =
  match s with
      ASTEcho e -> (match (eval_expr e c m) with
      Z(n) -> string_of_int n)


and eval_def d c m = 
match d with 
    ASTconst(x,_,e) -> (x, (eval_expr e c m)) :: c
    |ASTfunDef(x,t,a,e) -> (x,F(e,(eval_args a),c)) ::c
    |ASTfunRecDef(x,t,a,e) -> (x,Fr(e,x,(eval_args a),c)) ::c


(* *********************************** *)


and eval_cmd cmd c m=
  match cmd with
     ASTDef d -> eval_def d c m
       
    |ASTStat _ -> c


and eval_cmds cs c m=
  match cs with
    [] -> ()
    |[ASTStat(a)] -> Printf.printf "%s" (eval_stat a c m) 
    | a::b -> let c1 =  (eval_cmd a c m) in eval_cmds b c1 m
	

(* *********************************** *)



and eval_prog p = eval_cmds p ev_env mem_env
;;
	
let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0