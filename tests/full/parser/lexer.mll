(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, unknown characters
   and raises End_of_file on EOF. *)
{
  open Rpncalc (* Assumes the parser file is "rpncalc.mly". *)
}
let digit = ['0'-'9']
rule token = parse
  | [' ' '\t']  { token lexbuf }
  | '\n'    { NEWLINE }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
        { NUM (float_of_string num) }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MULTIPLY }
  | '/'     { DIVIDE }
  | '^'     { CARET }
  | 'n'     { UMINUS }
  | _       { token lexbuf }
  | eof     { raise End_of_file }
