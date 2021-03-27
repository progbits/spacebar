(* Page 314 *)
type statement =
  | BlockStatement
  | VariableStatement
  | EmptyStatement
  | ExpressionStatement
  | IfStatement
  | BreakableStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | LabelledStatement
  | ThrowStatement
  | TryStatement
  | DebuggerStatement

(* Page 325 *)
type binding_list = []

type lexical_declaration = Let of binding_list | Const of binding_list

type function_declaration =
  {binding_identifier: string; formal_parameters: string; function_body: string}

(* Page 315 *)
type declaration =
  | HoistableDeclaration
  | ClassDeclaration
  | LexicalDeclaration

(* Page 318 *)
type statement_list_item = Statement of statement | Declaration of declaration

(* AST root *)
type script = function_declaration

let print_token token =
  match token with
  (* Keywords *)
  | Parser.ALIGNOF -> print_endline "ALIGNOF"
  | Parser.AUTO -> print_endline "AUTO"
  | Parser.BREAK -> print_endline "BREAK"
  | Parser.CASE -> print_endline "CASE"
  | Parser.CHAR -> print_endline "CHAR"
  | Parser.CONST -> print_endline "CONST"
  | Parser.CONTINUE -> print_endline "CONTINUE"
  | Parser.DEFAULT -> print_endline "DEFAULT"
  | Parser.DO -> print_endline "DO"
  | Parser.DOUBLE -> print_endline "DOUBLE"
  | Parser.ELSE -> print_endline "ELSE"
  | Parser.ENUM -> print_endline "ENUM"
  | Parser.EXTERN -> print_endline "EXTERN"
  | Parser.FLOAT -> print_endline "FLOAT"
  | Parser.FOR -> print_endline "FOR"
  | Parser.GOTO -> print_endline "GOTO"
  | Parser.IF -> print_endline "IF"
  | Parser.INLINE -> print_endline "INLINE"
  | Parser.INT -> print_endline "INT"
  | Parser.LONG -> print_endline "LONG"
  | Parser.REGISTER -> print_endline "REGISTER"
  | Parser.RESTRICT -> print_endline "RESTRICT"
  | Parser.RETURN -> print_endline "RETURN"
  | Parser.SHORT -> print_endline "SHORT"
  | Parser.SIGNED -> print_endline "SIGNED"
  | Parser.SIZEOF -> print_endline "SIZEOF"
  | Parser.STATIC -> print_endline "STATIC"
  | Parser.STRUCT -> print_endline "STRUCT"
  | Parser.SWITCH -> print_endline "SWITCH"
  | Parser.TYPEDEF -> print_endline "TYPEDEF"
  | Parser.UNION -> print_endline "UNION"
  | Parser.UNSIGNED -> print_endline "UNSIGNED"
  | Parser.VOID -> print_endline "VOID"
  | Parser.VOLATILE -> print_endline "VOLATILE"
  | Parser.WHILE -> print_endline "WHILE"
  | Parser.ALIGNAS -> print_endline "ALIGNAS"
  | Parser.ATOMIC -> print_endline "ATOMIC"
  | Parser.BOOL -> print_endline "BOOL"
  | Parser.COMPLEX -> print_endline "COMPLEX"
  | Parser.GENERIC -> print_endline "GENERIC"
  | Parser.IMAGINARY -> print_endline "IMAGINARY"
  | Parser.NORETURN -> print_endline "NORETURN"
  | Parser.STATIC_ASSERT -> print_endline "STATIC_ASSERT"
  | Parser.THREAD_LOCAL -> print_endline "THREAD_LOCAL"
  (*Other tokens.*)
  | Parser.IDENTIFIER _ -> print_endline "IDENTIFIER"
  | Parser.CONSTANT _ -> print_endline "CONSTANT"
  | Parser.STRING_LITERAL _ -> print_endline "STRING_LITERAL"
  (*Punctuators.*)
  | Parser.LBRACE -> print_endline "LBRACE"
  | Parser.RBRACE -> print_endline "RBRACE"
  | Parser.LPAREN -> print_endline "LPAREN"
  | Parser.RPAREN -> print_endline "RPAREN"
  | Parser.LBRACKET -> print_endline "LBRACKET"
  | Parser.RBRACKET -> print_endline "RBRACKET"
  | Parser.DOT -> print_endline "DOT"
  | Parser.STAR -> print_endline "STAR"
  | Parser.SEMICOLON -> print_endline "SEMICOLON"
  | Parser.ASSIGN -> print_endline "ASSIGN"
  | Parser.COMMA -> print_endline "COMMA"
  | Parser.EOF -> print_endline "EOF"
  | _ -> print_endline "Unknown"

let print_tokens lexbuf =
  try
    while true do
      let token = Lexer.token lexbuf in
      match token with
      | Parser.EOF -> print_endline "EOF" ; raise End_of_file
      | _ -> print_token token
    done
  with End_of_file -> ()

(* let bar =
  let lexbuf = Lexing.from_channel stdin in
  print_tokens lexbuf
 *)

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.translation_unit Lexer.token lexbuf in
    Ast.print_translation_unit program
  with End_of_file -> exit 0
