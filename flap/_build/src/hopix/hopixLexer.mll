{
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


}



let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let alpha = ['0'-'9' 'A'-'Z' 'a'-'z' ]

let alien_infix_id = '\'' ['A'-'Z' 'a'-'z' '+' '-' '*' '/' '<' '=' '>' '_'] '\''

let alien_prefix_id = '\'' ['A'-'Z' 'a'-'z' '+' '-' '*' '/' '<' '=' '>' '_']

let var_id = ['a'-'z'] ['0'-'9' 'A'-'Z' 'a'-'z' '_' ]* | alien_prefix_id

let constr_id = ['A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_con = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = '\'' ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let integer = ['0'-'9']+ |'0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']+ | '0' ['B' 'b']['0'-'1']+ | '0' ['O' 'o']['0'-'7']+

let atom = ['\000' - '\255'] | '\\' '0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']  
            | '\\' '0' ['B' 'b']['0'-'1']+ | '\\' '0' ['O' 'o']['0'-'7']+ (* | [printable] *)

let char = '\'' atom '\''

 
rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | "--"            { comment_line lexbuf }
  | "{-"            { comment_block 0 lexbuf }


  (** Symbols *)
  | "="       { EQUAL }

  (** Keywords *)
  | "val"           { VAL   }
  | "if"            { IF    }
  | "then"          { THEN  }
  | "else"          { ELSE  }

  (** Identifiers *)
  | type_variable as i  { Type_Var i}

  (** Operators *)
  | "*"       { STAR  }
  | "+"       { PLUS  }
  | "-"       { MINUS }
  | "/"       { SLASH }


  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }


 and comment_block lvl = parse 
| "{-"    { comment_block (lvl + 1) lexbuf       }
| "-}"    { 
            if lvl = 0
            then  token lexbuf
            else comment_block (lvl - 1) lexbuf
                                            }
| _       { comment_block lvl lexbuf}
| eof     { error lexbuf "comment unclosed" }


 and comment_line = parse
| _               { comment_line  lexbuf} 
| newline | eof   { token lexbuf }
