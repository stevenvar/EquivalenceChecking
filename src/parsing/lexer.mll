
{
  open Parser

  exception Error of string

}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| ";;"
    { DOUBLESEMICOLON }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| "()"
  { UNIT }
| "let"
    { LET }
| "rec"
    { REC }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { MUL }
| "=="
    { DOUBLEEQUAL }
| "if"
    { IF }
| "in"
    { IN }
| "then"
    { THEN }
| "else"
    { ELSE }
| ';'
    { SEMICOLON }
| "fun"
    { FUN }
| '='
    { EQUAL }
| "->"
   { ARROW }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }
| ['a'-'z']+ as s
    { STR s }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
