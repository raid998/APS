(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMICOLON }
  | ':'              { COLON }
  | ','              { COMMA }
  | '*'              { STAR }
  | "->"             { ARROW }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "SET"            { SET }
  | "IF"             { IFF }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "ECHO"           { ECHO }
  | "CONST" { CONST }
  | "FUN" { FUN }
  | "REC" { REC }
  | "if" { IF }
  | "and" { AND }
  | "or" { OR }
  | "bool" { BOOL }
  | "int" { INT }  
  | "void" { VOID } 
  | "adr" { ADR } 
  | "var" { VAR2 }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
