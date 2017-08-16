{
    open Parser
}

rule lex = parse
  | [' ' '\t']+  { lex lexbuf }
  | "/**"        { comment lexbuf }
  | "//"         { comment_line lexbuf}
  | "\n"        { lex lexbuf }
  | "^"         { AND }
  | "false"     { FALSE }
  | "true"      { TRUE }
  | "Tell"      { TELL }
  | "||"        { PARALLEL }
  | "Next"      { NEXT }
  | ['0'-'9']['0'-'9']* as i     { INT (int_of_string (i)) }
  | ['a'-'z']['0'-'9' 'a'-'z']* as s { CONSTRAINT (s) }
  | "("         { OP }
  | ")"         { CL }
  | "{"         { BOP }
  | "}"         { BCL }
  | ":"         { COLON }
  | "*"         { STAR }
  | "!"         { BANG }
  | "Unless"    { UNLESS }
  | "Choice"    { CHOICE }
  | "When"      { WHEN }
  | "+"         { PLUS }
  | "^"         { EXP }
  | eof         { EOF }


and comment = parse
    | "**/"        { lex lexbuf   }
    | _             { comment lexbuf }

and comment_line = parse
    | "\n"          { lex lexbuf   }
    | _             { comment_line lexbuf }
