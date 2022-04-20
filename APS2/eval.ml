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
    | A of int
    | P of cmd list*string list*((string*v) list)
    | Pr of cmd list*string*string list*((string*v) list)
    | B of v*int

let string_of_v v = match v with 
  Z(n) -> "Z(" ^ ((string_of_int n) ^ ")")
  |F(_,_,_) -> "F"
  |Fr(_,_,_,_) -> "Fr"
  |A(n) -> "A(" ^ ((string_of_int n) ^ ")")
  |P(_,_,_) -> "P"
  |Pr(_,_,_,_) ->"Pr"
let (ev_env:(string*v) list) = [("true",Z(1));("false",Z(0))];;

let mem_counter = ref 0;;

let (mem_env: (int*v option) list) = [];;

let alloc mem = let res =  (!mem_counter, ((!mem_counter, None)::mem)) in mem_counter := (!mem_counter + 1) ; res;;

let allocn mem n = let a = !mem_counter in let rec allocn_aux mem1 n d = if d < n then (
  let m = (!mem_counter,None)::mem1 in
  mem_counter := (!mem_counter + 1);
  allocn_aux m n (d+1)
) else mem1 in let m = allocn_aux mem n 0 in (a,m)

let rec find_from_mem a mem =
  match mem with 
  [] -> failwith "address not in memory"
  |(x,v)::rest -> if x = a then (
    match v with 
    None -> failwith "undefined"
    |Some v1 -> v1
  ) else find_from_mem a rest
(* ------------------------------------------------ *)

let rec is_defined_in_mem a mem =
  match mem with 
  [] -> failwith "not assigned"
  |(x,v)::rest -> if x = a then true else is_defined_in_mem a rest

let rec is_defined x e m = 
match e with 
   [] -> false
  | (a,v)::rest -> if (String.equal a x) then (
    match v with 
    A(i) -> is_defined_in_mem i m
    |_ -> failwith "must be a variable"
    ) else is_defined x rest m

let rec get_val x e m = 
  match e with 
   [] -> failwith x
  | (a,v)::rest -> if (String.equal a x) then (
    match v with 
    A(i) -> find_from_mem i m
    |_ -> v
    ) else get_val x rest m

let set_val x v m = List.map (fun elem -> if (fst elem) = x then (x,Some v) else elem) m

let rec find_x x e= 
  match e with 
   [] -> failwith x
  | (a,v)::rest -> if (String.equal a x) then v else find_x x rest

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
  |Argp(a,_)::xs -> a::(extractArgs xs)
  |Argpv(a,_)::xs -> a::(extractArgs xs)
  

(* ------------------------------------------------ *)
let rec eval_arg a = match a with 
  Argu(i,t) -> i
   and  eval_args a = match a with
  [] -> []
  |[b] -> [eval_arg(b)]
  |b::bs -> eval_arg b::(eval_args bs)
    
   and eval_expr e c m : (v*((int*v option) list)) =  match e with
      ASTNum n -> (Z(n),m)    
      | ASTApp(e1, es) -> 
      (match e1 with 
        ASTId("not") -> (eval_not (fst (eval_expr (List.hd es) c m)),m) 
       |ASTId("add") ->  ((eval_add (fst(eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       |ASTId("sub") ->  ((eval_sub (fst (eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       |ASTId("mul") ->  ((eval_mul (fst (eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       |ASTId("div") ->  ((eval_div (fst (eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       |ASTId("eq") ->  ((eval_eq (fst (eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       |ASTId("lt") ->  ((eval_lt (fst (eval_expr (List.hd es) c m)) (fst (eval_expr (List.hd (List.tl es)) c m))),m)
       | _ -> let closure = (fst (eval_expr e1 c m)) in (match closure with
      |Z(_) -> failwith "Pas une fonction"
      |F(body,vars,sc) -> let vals = List.map fst (eval_exprs es c m) in ((fst(eval_expr body (List.append (List.combine vars vals) sc ) m)),m)
      |Fr(body,name,vars,sc) -> let vals = List.map fst (eval_exprs es c m) in ((fst (eval_expr body (List.append (List.append (List.combine vars vals) [(name,Fr(body,name,vars,sc))]) sc ) m),m))
        )
      )
    
    | ASTId(x) -> ((get_val x c m),m)
    
    | ASTif(condition,body,alternant) -> if (fst (eval_expr condition c m) = Z(1)) then (fst (eval_expr body c m),m) else (fst (eval_expr alternant c m),m)
    | ASTfun(args,e1) -> (F(e1,(eval_args args),c),m)
    | ASTand(a,b) -> let (x,m1) =  (eval_expr a c m) in  if x = Z(1) then (eval_expr b c m1) else (Z(0),m1)
    | ASTor(a,b) -> let (x,m1) =  (eval_expr a c m) in if x = Z(1) then ( Z(1),m1) else (eval_expr b c m1)
    | ASTAlloc(e) -> let (Z(n), m1) = eval_expr e c m in let (a,m2) = allocn m1 n in (B(A(a),n),m2)



and eval_exprs es c m : ((v * (int * v option) list) list)  =
  match es with
      [] -> []
    | [e] -> [eval_expr e c m] 
    | e::es -> (eval_expr e c m)::(eval_exprs es c m)
and eval_exprp e c m = 
        match e with 
            ASTExpr(ASTId(x)) -> (find_x x c,m)
          | ASTExpr(e1) -> eval_expr e1 c m
          | ASTAdr(a) -> (match (find_x a c) with A(i) -> (A(i),m) | _ -> failwith "not a reference")
and eval_exprsp es c m = 
        match es with 
            [] -> []
          | [e] -> [eval_exprp e c m]
          | e::es -> (eval_exprp e c m) ::(eval_exprsp es c m)
and eval_lval lval c m = 
match lval with
        ASTLval(s) -> (match (find_x s c) with 
            A(a) -> (a,m)
          | _ -> failwith "invalid identifier"
        )
      | ASTNthL(lval1, e) -> (
        match lval1 with 
          ASTLval(x1) -> (match (find_x x1 c) with 
            B(A(a),n) -> (match (eval_expr e c m) with
                (Z(i),m1) -> ((a+i),m1)
              | _ -> failwith "error lnth1"
              )
            | _ -> failwith (x1 ^ " is not an array\n"))
        | _ -> (let (a1,m1) = eval_lval lval1 c m in 
          match find_from_mem a1 m1 with 
          B(A(a2),_) -> (match eval_expr e c m1 with
              (Z(i),m2) -> ((a2+i,m2))
            | _ -> failwith "index must be an integer"
          )
          | _ -> failwith "undefined in memory"
        ))
            
and eval_stat s c m f=
  match s with
      ASTEcho e ->  (match (fst(eval_expr e c m)) with
      Z(n) -> (match f with "" -> (m,(string_of_int n)) | _ -> (m,(string_of_int n) ^("." ^ f))))
    | ASTSet(x,e) ->let (v,m1) = eval_expr e c m  in let (a1,m2) = eval_lval x c m in (
        (set_val a1 v m2,f)
    )
    | ASTIff(e,bk1,bk2) -> if (fst (eval_expr e c m) = Z(1)) then eval_cmds bk1 c m f else eval_cmds bk2 c m f
    | ASTloop(e,bk) -> if (fst (eval_expr e c m) = Z(1)) then let (m1,f1) = eval_cmds bk c m f in   eval_stat s c m1 f else (m,f)
    | ASTCall(x,es) -> let x1 = find_x x c in (
        match x1 with 
          P(bk,args,c1) -> let values = List.map fst (eval_exprsp es c m) in let c2 = (List.combine args values) @ c1 in eval_cmds bk c2 m f 
        | Pr(bk,n,args,c1) ->  let values = List.map fst (eval_exprsp es c m) in let c2 = (List.combine args values)@((n,Pr(bk,n,args,c1))::c1) in eval_cmds bk c2 m f 
        | _ -> failwith "not callable"
    )

and eval_def d c m = 
match d with 
      ASTconst(x,_,e) -> let (v,m1) = eval_expr e c m in ((x,v) :: c,m1)
    | ASTfunDef(x,_,a,e) -> ((x,F(e,(eval_args a),c)) ::c, m)
    | ASTfunRecDef(x,_,a,e) -> ((x,Fr(e,x,(eval_args a),c)) ::c, m)
    | ASTVar(x,_) -> let (a,mem) = (alloc m) in (((x,A(a))::c), mem)
    | ASTProc(x,a,bk) -> (((x,P(bk,(extractArgs a),c))::c),m)
    | ASTProcRec(x,a,bk) -> (((x,Pr(bk,x,(extractArgs a),c))::c),m)


(* *********************************** *)


and eval_cmd cmd c m f=
  match cmd with
     ASTDef d -> ((eval_def d c m),f)
    
    |ASTStat s -> let (m1,f1) = eval_stat s c m f in  ((c,m1),f1)


and eval_cmds cs c m f=
  match cs with
    [] ->  (m,f)
    | a::b -> let c1 =  (eval_cmd a c m f) in eval_cmds b (fst (fst c1)) (snd (fst c1)) (snd c1)
	

(* *********************************** *)



and eval_prog p = let (_,f) =  eval_cmds p ev_env mem_env "" in Printf.printf "%s" f 
;;
	
let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0
