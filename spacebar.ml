let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.translation_unit Lexer.token lexbuf in
    let outfile = open_out "output.ws" in
    let bytecode = Generate.generate program in
    List.iter (Whitespace.print outfile) bytecode
  with End_of_file -> exit 0
