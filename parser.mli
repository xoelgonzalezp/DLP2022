type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | STRING
  | UNIT
  | UNITV
  | LCOR
  | RCOR
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LIST
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | COMMA
  | SEMICOLON
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | QM
  | CONCAT
  | ID of (string)
  | INTV of (int)
  | STRINGV of (string)
  | STRINGT of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
