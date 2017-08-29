{
  open Parser
  exception Error of string

  let unterminated_comment () =
    raise (Error "unterminated comment")
  let unterminated_string () =
    raise (Error "unterminated string")
}

let blank = [' ' '\t' '\r' '\n']
let newline = '\n'
let chars = ['a'-'z' 'A'-'Z' '0'-'9']

rule lex = parse
  | blank+  { lex lexbuf }
  | "(*"    { comment lexbuf; lex lexbuf }
  | newline { Lexing.new_line lexbuf; lex lexbuf }
  | eof     { EOF }
  | "."     { DOT }
  | "="     { EQ }
  | ":="    { DEF }
  | "<>"    { INEQ }
  | "/\\"   { LAND }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { STAR }
  | "/"     { DIV }
  | "^"     { EXP }
  | "^T"    { TRANSPOSE }
  | "diag"  { DIAG }
  | "["     { LBRACK }
  | "]"     { RBRACK }
  | "{"     { LCURLY }
  | "}"     { RCURLY }
  | "\\"    { BACKSLASH }
  | "("     { LPAR }
  | ")"     { RPAR }
  | ","     { COMMA }
  | ";"     { SEMICOLON }
  | ":"     { COLON }
  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "sum"    { SUM }
  | "prod"   { PROD }
  | "_"      { UNDERSCORE }
  | "in"     { IN }
  | "sets"   { SETS }
  | "vars"   { VARS }
  | "params" { PARAMS }
  | "Zp"     { ZP }
  | "goal"   { GOAL }
  | "substitute" { SUBSTITUTE }
  | "by"         { BY }
  | "simplify"   { SIMPLIFY }
  | "full_simplify" { FULLSIMPLIFY }
  | "contradiction" { CONTRADICTION }
  | "extract_coeff" { EXTRACTCOEFF }
  | "go"     { GO }
  | "add_equation"  { ADDEQUATION }
  | "o"      { CIRC }
  | "{0,1}"  { ZO }

  | '-'?['0'-'9']['0'-'9']* as s { INT(int_of_string(s)) }
  | ['a'-'z' 'A'-'Z' '\\']chars* as s { NAME(s) }

and comment = parse
  | "*)"        { () }
  | "(*"        { comment lexbuf; comment lexbuf }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }
