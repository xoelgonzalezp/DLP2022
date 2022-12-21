

{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "Unit"      { UNIT }
  | "()"        { UNITV }
  | "unit"      { UNITV }
  | '['         { LCOR }
  | ']'         { RCOR }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         {LBRACKET}
  | '}'         {RBRACKET}
  | "List"      {LIST}
  | "nil"       {NIL}
  | "cons"      {CONS}
  | "isnil"     {ISNIL}
  | "head"      {HEAD}
  | "tail"      {TAIL}
  | ','         {COMMA}
  | ';'         { SEMICOLON }
  | '.'         { DOT }
  | '='         { EQ }
  | '^'         { CONCAT }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "\""        { QM }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | '\"'[^';''\"''\'']*'\"'
                { STRINGT (
                  let s = Lexing.lexeme lexbuf
                  in String.sub s 1 ((String.length s)-2)
                ) }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
                { ID (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

