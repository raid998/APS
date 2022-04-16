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

let rec print_argp a = 
  match a with 
      Argp(i,t) -> (
        Printf.printf "argp(";
        print_expr (ASTId(i));
        Printf.printf ", ";
        print_type t;
        Printf.printf ")"
      )
    | Argpv(i,t) -> (
      Printf.printf "argp(";
      print_expr (ASTId(i));
      Printf.printf ", ref(";
      print_type t;
      Printf.printf "))"
    )

  and print_argsp a =
    match a with 
      [] -> ()  
    | b -> (
      Printf.printf "argsp([";
      let rec print_argsp_aux x =
        match x with 
            [] -> ()
          | [c] -> (print_argp c)
          | c::d -> (
            print_argp c;
            Printf.printf ",";
            print_argsp_aux d
          ) in print_argsp_aux b;
      Printf.printf "])"
      )
    
and print_arg a = match a with 
  Argu(i,t) -> (
    Printf.printf"arg(";
    print_expr (ASTId(i));
    Printf.printf", ";
    print_type t;
    Printf.printf")";
  ) and  print_args a = match a with
  [] -> ()
  |[b] -> (
    Printf.printf"args([";
    print_arg b;
    Printf.printf"])";
    );
  |b -> (
    let rec print_args_aux l = match l with
      [] -> ()
      |[le] -> (print_arg le)
      | lh::lt -> (
        print_arg lh;
        Printf.printf", ";
        print_args_aux lt;
      )
      in
      Printf.printf"args([";
      print_args_aux b;
      Printf.printf"])";
    
  ) and print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTApp(e, es) -> (
	Printf.printf"app(";
	print_expr e;
	Printf.printf",[";
	print_exprs es;
	Printf.printf"])"
      )
    | ASTif(condition,body,alternant) -> (
      Printf.printf "if(";
      print_expr condition;
      Printf.printf", ";
      print_expr body;
      Printf.printf ", ";
      print_expr alternant;
      Printf.printf")";
    )
    |ASTfun(args,e) -> (
      Printf.printf"fun(";
      print_args args;
      Printf.printf ", ";
      print_expr e;
      Printf.printf ")";
    )
    |ASTand(a,b) -> (
      Printf.printf"and(";
      print_expr a;
      Printf.printf", ";
      print_expr b;
      Printf.printf")";
    )
    |ASTor(a,b) -> (
      Printf.printf"or(";
      print_expr a;
      Printf.printf", ";
      print_expr b;
      Printf.printf")";
    )
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
	print_expr e;
	print_char ',';
	print_exprs es
      )

and print_exprp e =
  match e with 
      ASTExpr(e1) -> print_expr e1
    | ASTAdr(a) -> Printf.printf "adr(%s)" a

and print_exprsp es = 
  match es with
       [] -> ()
     | [e] -> print_exprp e
     | e::es -> (
       print_exprp e;
       Printf.printf ",";
       print_exprsp es
     )
and print_stat s =
  match s with
      ASTEcho e -> (
	Printf.printf("echo(");
	print_expr(e);
	Printf.printf(")")
      )
      |ASTSet(x,e) -> (
        Printf.printf "set(";
        print_expr (ASTId(x));
        Printf.printf ",";
        print_expr e;
        Printf.printf ")"
      )
      |ASTIff(e,bk1,bk2) -> (
        Printf.printf "iff(";
        print_expr e;
        Printf.printf ",[";
        print_cmds bk1;
        Printf.printf "],[";
        print_cmds bk2;
        Printf.printf "])";
      )
      |ASTloop(e,bk) -> (
        Printf.printf "while(";
        print_expr e;
        Printf.printf",[";
        print_cmds bk;
        Printf.printf "])"
      )
      |ASTCall(x,es) -> (
        Printf.printf "call(";
        print_expr (ASTId(x));
        Printf.printf ",[";
        print_exprsp es;
        Printf.printf "])";
      )

and print_type t = 
  match t with 
    Bool -> Printf.printf"bool"
    |Int -> Printf.printf"int"
    |Void -> Printf.printf"void"
    |FuncT(ts) -> (
      Printf.printf"types([";
      let rec pta x = 
            match x with 
            [] -> ()
            |[Bool] -> Printf.printf"],bool)"
            |[Int] -> Printf.printf"],int)"
            |[Void] -> Printf.printf"],void"
            |Bool::c -> (
              if (List.length c == 1)then (Printf.printf "bool") else
              Printf.printf"bool, ";
              pta c;
            )
            |Int::c -> (
              if (List.length c == 1)then (Printf.printf "int") else
              Printf.printf"int, ";
              pta c;
            )
            |Void::c -> (
              if (List.length c == 1)then (Printf.printf "void") else
              Printf.printf"void, ";
              pta c;
            )
            |[FuncT([])] -> ()
            |[FuncT([c])] -> (
              pta [c];
            )
            |[FuncT(c::d)] -> (
              Printf.printf"types([";
              pta [c];
              Printf.printf", ";
              pta d;
            )
            |FuncT([])::c -> pta c
            |FuncT([c])::d -> (
              Printf.printf"types([";
              pta [c];
              Printf.printf", ";
              pta d;
            )
            |FuncT(c)::d -> (
              Printf.printf"types([";
              pta c;
              Printf.printf", ";
              pta d;
            )
            in pta ts;
          
        )
    

and print_def d = 
match d with 
    ASTconst(i,t,e) -> (
      Printf.printf "const(";
      print_expr (ASTId(i));
      Printf.printf", ";
      print_type t;
      Printf.printf ", ";
      print_expr e;
      Printf.printf")";
    )
    |ASTfunDef(i,t,a,e) -> (
      Printf.printf"funDef(";
      print_expr (ASTId(i));
      Printf.printf", ";
      print_type t;
      Printf.printf", ";
      print_args a;
      Printf.printf", ";
      print_expr e;
      Printf.printf")"
    )
    |ASTfunRecDef(i,t,a,e) -> (
      Printf.printf"funRecDef(";
      print_expr (ASTId(i));
      Printf.printf", ";
      print_type t;
      Printf.printf", ";
      print_args a;
      Printf.printf", ";
      print_expr e;
      Printf.printf")"
    )
    |ASTVar(x,t) -> (
      Printf.printf "var(";
      print_expr (ASTId(x));
      Printf.printf ",";
      print_type t;
      Printf.printf ")"
    )
    |ASTProc(x,a,bk) -> (
      Printf.printf "proc(";
      print_expr (ASTId(x));
      Printf.printf ",";
      print_argsp a;
      Printf.printf ",[";
      print_cmds bk;
      Printf.printf "])"
    )
    |ASTProcRec(x,a,bk) -> (
      Printf.printf "procRec(";
      print_expr (ASTId(x));
      Printf.printf ",";
      print_argsp a;
      Printf.printf ",[";
      print_cmds bk;
      Printf.printf "])"
    )

and print_cmd c =
  match c with
      ASTStat s -> print_stat s
     |ASTDef d -> print_def d



and print_cmds cs =
  match cs with
    [] -> ()
    |c::[] -> print_cmd c
    | a::b -> (
      print_cmd a;
      Printf.printf", ";
      print_cmds b;
    )
	
and print_prog p =
  Printf.printf("prog([");
  print_cmds p;
  Printf.printf("])")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
