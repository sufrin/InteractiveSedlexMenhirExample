
open Parsing

let process lexbuf =
  let lexer = SexpLexer.lexer lexbuf in
  try
      match parse lexer lexbuf with
      | OK ast  ->  
        (match ast with Sexp.EndFile -> raise EndFile | _ -> ());
        Format.fprintf Format.std_formatter "%a\n%!" Sexp.pp_sexp ast
      | ERR (pos, msg) ->  
        Format.fprintf Format.err_formatter "*** %a %s%!"   Sexp.pp_fpos pos msg
  with 
     (* abandon the current phrase on a lexer error *)
     SexpLexer.LexError (pos, msg) ->
         Format.fprintf Format.err_formatter "*** Lexing error: %s at %a\n%!" msg Sexp.pp_pos pos

let _ =
    let chan   = open_in "/dev/stdin" in
    let istty  = Unix.isatty @@ Unix.descr_of_in_channel chan in
    let lexbuf = Sedlexing.Utf8.from_channel ~chunk_size:(if istty then 1 else 1024) chan in
    Sedlexing.set_filename lexbuf "/dev/stdin";
    if istty then
       Sedlexing.set_prompter lexbuf (fun () -> Format.printf "> %!");
    try 
     while true do process lexbuf done
    with
     EndFile -> ()
      



