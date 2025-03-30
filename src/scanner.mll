{
    open Pparser
}

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { EQ }
| ';' { SEMI } 
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z']+ as id  { VARIABLE(id) } 
| eof { EOF }

