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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
