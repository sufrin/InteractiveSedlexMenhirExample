open Sedlexing

type token = SexpParser.token

open SexpParser

exception LexError of Lexing.position * string

let blank         = [%sedlex.regexp? ' ' | '\t']

let newline       = [%sedlex.regexp? '\r' | '\n' ]

let whitespace    = [%sedlex.regexp? Plus (blank | newline)]

let decimal_ascii = [%sedlex.regexp? Plus ('0' .. '9')]

let octal_ascii   = [%sedlex.regexp? "0o", Plus ('0' .. '7')]

let hex_ascii     = [%sedlex.regexp? "0x", Plus (('0' .. '9' | 'a' .. 'f' | 'A' .. 'F'))]

let idChunk       = [%sedlex.regexp? Compl (' ' | '\t' | '\n' | '\r' | '(' | ')' | '-' | '+' | '@' | '.' | '"') ]

let stringChunk   = [%sedlex.regexp? Star (Compl ('"' | '\\' | '\n'))]


let rec skipWhitespace buf =
  match%sedlex buf with
  | Plus whitespace -> skipWhitespace buf
  | _               -> ()

let string buf  =
  let buffer = Buffer.create 10 in
  let rec read_string buf  =
    match%sedlex buf with
    | eof                 -> err "End of file in string" buf
    | '\n'                -> err "End of line in string" buf
    | "\\\""              -> ins "\"" buf
    | "\\\\"              -> ins "\\" buf
    
    | "\\n"               -> ins "\n" buf
    | "\\\n", Star blank, "\\"  -> read_string buf
    | "\\\n", Star blank, stringChunk  -> ins (Utf8.lexeme buf) buf (* err "Wrong continuation line after \\ at line end in string" buf *)
    
    | '"'                 -> Buffer.contents buffer
    | stringChunk         -> ins (Utf8.lexeme buf) buf
    | _                   -> assert false
    and err s buf = raise @@ LexError (fst @@ lexing_positions buf,  s)
    and ins s buf = Buffer.add_string buffer s; read_string buf
  in
    read_string buf

let digit_value c =
  let open Stdlib in
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value buffer ~base ~first =
  let buf = Utf8.lexeme buffer in
  let c = ref 0 in
  for i = first to String.length buf - 1 do
    let v = digit_value buf.[i] in
   assert (v < base);
    c := (base * !c) + v
  done;
  !c

let token buf =
  skipWhitespace buf; 
  match%sedlex buf with
  | eof -> EOF
  | "@" -> AT
  | '-' -> MINUS
  | '+' -> PLUS
  | '.' -> DOT
  | '"' -> STRING(string buf)
  | '(' -> BRA
  | ')' -> KET
  | hex_ascii ->
      let number = num_value ~base:16 ~first:2 buf in
      INT number
  | octal_ascii ->
      let number = num_value ~base:8 ~first:2 buf in
      INT number
  | decimal_ascii ->
      let number = num_value ~base:10 ~first:0 buf in
      INT number
  | Plus idChunk -> 
    ID (Utf8.lexeme buf)
  | _ ->
    let pos  = fst @@ lexing_positions buf in
    let _    = Sedlexing.next buf in (* Skip the bad character: pretend it's a token *)
    let tok  = Utf8.lexeme buf in  
    raise @@ LexError (pos,  "Unexpected character: "^tok)

let lexer buf =
  Sedlexing.with_tokenizer token buf



