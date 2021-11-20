let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.translation_unit Lexer.token lexbuf in
    let bytecode = Generate.generate program in
    List.iter (Whitespace.print stdout) bytecode
  with End_of_file -> exit 0
