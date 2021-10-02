let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.translation_unit Lexer.token lexbuf in
    Ast.print_translation_unit program
  with End_of_file -> exit 0
