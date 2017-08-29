(** Convert lexer and parser errors to ParseError exception *)

exception LexerError of string
exception ParserError of string

let wrap_error f s =
  let sbuf = Lexing.from_string s in
  try
    f sbuf
  with
  | Lexer.Error msg ->
     raise (LexerError (Printf.sprintf "%s%!" msg))
  | Parser.Error ->
    let start = Lexing.lexeme_start sbuf in
    let err = Printf.sprintf
                "Syntax error at offset %d (length %d): parsed ``%s'',\nerror at ``%s''"
                start
                (String.length s)
                (if start >= String.length s then s  else (String.sub s 0 start))
                (if start >= String.length s then "" else (String.sub s start (String.length s - start)))
    in
    raise (ParserError err)
  | e ->
    let ex = Printexc.to_string e in
    let bt = Printexc.get_backtrace () in
    let err = Printf.sprintf "Unknown error while lexing/parsing: %s\n%s%!" ex bt in
    raise (ParserError err)

(** Parse type declaration. *)
let p_cmds = wrap_error (Parser.cmds_t Lexer.lex)
