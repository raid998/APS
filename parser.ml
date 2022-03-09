type token =
  | NUM of (int)
  | IDENT of (string)
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | SEMICOLON
  | COLON
  | COMMA
  | STAR
  | ARROW
  | CONST
  | FUN
  | REC
  | ECHO
  | IF
  | AND
  | OR
  | BOOL
  | INT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 39 "parser.ml"
let yytransl_const = [|
  259 (* LBRA *);
  260 (* RBRA *);
  261 (* LPAR *);
  262 (* RPAR *);
  263 (* SEMICOLON *);
  264 (* COLON *);
  265 (* COMMA *);
  266 (* STAR *);
  267 (* ARROW *);
  268 (* CONST *);
  269 (* FUN *);
  270 (* REC *);
  271 (* ECHO *);
  272 (* IF *);
  273 (* AND *);
  274 (* OR *);
  275 (* BOOL *);
  276 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\009\000\008\000\008\000\010\000\003\000\004\000\004\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\007\000\007\000\
\005\000\005\000\005\000\002\000\002\000\002\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\002\000\003\000\001\000\003\000\001\000\
\001\000\006\000\005\000\005\000\004\000\004\000\001\000\002\000\
\004\000\007\000\008\000\001\000\001\000\005\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\008\000\009\000\000\000\
\000\000\004\000\000\000\001\000\000\000\020\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\007\000\014\000\000\000\000\000\
\000\000\016\000\013\000\024\000\000\000\000\000\000\000\000\000\
\011\000\012\000\022\000\018\000\000\000\010\000\019\000"

let yydgoto = "\002\000\
\046\000\035\000\028\000\029\000\008\000\036\000\047\000\009\000\
\004\000\010\000"

let yysindex = "\005\000\
\006\255\000\000\016\255\000\000\011\255\020\255\039\255\008\255\
\010\255\000\000\007\255\007\255\030\255\000\000\000\000\031\255\
\002\255\000\000\016\255\000\000\007\255\000\000\000\000\039\255\
\040\255\007\255\037\255\038\255\042\255\039\255\039\255\039\255\
\039\255\000\000\041\255\043\255\000\000\031\255\046\255\007\255\
\031\255\039\255\039\255\039\255\039\255\039\255\044\255\007\255\
\007\255\048\255\031\255\000\000\000\000\000\000\039\255\047\255\
\050\255\000\000\000\000\000\000\051\255\039\255\054\255\053\255\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\056\255\000\000\000\000\000\000\000\000\
\000\000\000\000\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\246\255\000\000\226\255\000\000\016\000\019\000\047\000\
\000\000\000\000"

let yytablesize = 66
let yytable = "\018\000\
\024\000\025\000\014\000\015\000\016\000\001\000\017\000\050\000\
\003\000\033\000\053\000\021\000\011\000\020\000\019\000\039\000\
\037\000\030\000\031\000\032\000\063\000\012\000\043\000\044\000\
\045\000\022\000\023\000\005\000\006\000\052\000\007\000\026\000\
\027\000\013\000\054\000\055\000\056\000\057\000\061\000\014\000\
\015\000\016\000\038\000\017\000\040\000\042\000\041\000\064\000\
\051\000\059\000\048\000\062\000\065\000\049\000\068\000\066\000\
\067\000\069\000\070\000\006\000\015\000\071\000\023\000\060\000\
\058\000\034\000"

let yycheck = "\007\000\
\011\000\012\000\001\001\002\001\003\001\001\000\005\001\038\000\
\003\001\017\000\041\000\005\001\002\001\004\001\007\001\026\000\
\024\000\016\001\017\001\018\001\051\000\002\001\030\000\031\000\
\032\000\019\001\020\001\012\001\013\001\040\000\015\001\002\001\
\002\001\014\001\042\000\043\000\044\000\045\000\049\000\001\001\
\002\001\003\001\003\001\005\001\008\001\004\001\009\001\055\000\
\003\001\006\001\010\001\004\001\006\001\011\001\062\000\006\001\
\006\001\004\001\006\001\004\001\006\001\069\000\011\001\048\000\
\046\000\019\000"

let yynames_const = "\
  LBRA\000\
  RBRA\000\
  LPAR\000\
  RPAR\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  STAR\000\
  ARROW\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 35 "parser.mly"
                        ( _2 )
# 175 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 39 "parser.mly"
                        ( [ASTStat _1] )
# 182 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 40 "parser.mly"
                          (ASTDef(_1)::_3)
# 190 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 44 "parser.mly"
                        ( ASTEcho(_2) )
# 197 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 47 "parser.mly"
                  (Argu(_1, _3))
# 205 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 50 "parser.mly"
      ([_1])
# 212 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 51 "parser.mly"
                  (_1::_3)
# 220 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
                        ( ASTNum(_1) )
# 227 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTId(_1) )
# 234 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                              (ASTif(_3,_4,_5))
# 243 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                          (ASTand(_3,_4))
# 251 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                         (ASTor(_3,_4))
# 259 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTApp(_2, _3) )
# 267 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                      ( ASTfun(_2,_4) )
# 275 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
             ( [_1] )
# 282 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
             ( _1::_2 )
# 290 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                        ( ASTconst(_2,_3,_4) )
# 299 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                                      (ASTfunDef(_2,_3,_5,_7))
# 309 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                                          (ASTfunRecDef(_3,_4,_6,_8))
# 319 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
       ( Bool )
# 325 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
        ( Int )
# 331 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typ list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 75 "parser.mly"
                              (FuncT(_2 @ [_4]))
# 339 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 78 "parser.mly"
     ([_1])
# 346 "parser.ml"
               : Ast.typ list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ list) in
    Obj.repr(
# 79 "parser.mly"
                  (_1 :: _3)
# 354 "parser.ml"
               : Ast.typ list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
