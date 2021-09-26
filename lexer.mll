{
    open Parser

    let keywords = [
        ("alignof", ALIGNOF);
        ("auto", AUTO);
        ("break", BREAK);
        ("case", CASE);
        ("char", CHAR);
        ("const", CONST);
        ("continue", CONTINUE);
        ("default", DEFAULT);
        ("do", DO);
        ("double", DOUBLE);
        ("else", ELSE);
        ("enum", ENUM);
        ("extern", EXTERN);
        ("float", FLOAT);
        ("for", FOR);
        ("goto", GOTO);
        ("if", IF);
        ("inline", INLINE);
        ("int", INT);
        ("long", LONG);
        ("register", REGISTER);
        ("restrict", RESTRICT);
        ("return", RETURN);
        ("short", SHORT);
        ("signed", SIGNED);
        ("sizeof", SIZEOF);
        ("static", STATIC);
        ("struct", STRUCT);
        ("switch", SWITCH);
        ("typedef", TYPEDEF);
        ("union", UNION);
        ("unsigned", UNSIGNED);
        ("void", VOID);
        ("volatile", VOLATILE);
        ("while", WHILE);
        ("_alignas", ALIGNAS);
        ("_atomic", ATOMIC);
        ("_bool", BOOL);
        ("_complex", COMPLEX);
        ("_generic", GENERIC);
        ("_imaginary", IMAGINARY);
        ("_noreturn", NORETURN);
        ("_static_assert", STATIC_ASSERT);
        ("_thread_local", THREAD_LOCAL);
    ]

    let is_keyword word =
        let found = List.find_opt (fun keyword -> (fst keyword) = word) keywords in
        match found with
        | Some keyword -> Some (snd keyword)
        | None -> None
}

(* 6.4.2 *)
let identifier = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

(* 6.4.4 *)
let integer_constant = ['0'-'9']+

rule token = parse
    | [' '  '\t' '\n'] { token lexbuf }
    | integer_constant as i
        { CONSTANT (int_of_string i) }
    | identifier as id { 
        match is_keyword id with
        | Some keyword -> keyword
        | None -> IDENTIFIER id
    }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "." { DOT }
    (* | "->" ->
    | "++" ->
    | "--" ->
    | "&" ->  *)
    | "*" { STAR }
    | "+" { PLUS }
    | "-" { MINUS }
    (* | "~" ->
    | "!" -> *)
    | "/" { SLASH }
    | "%" { MOD }
    (* | "<<" ->
    | ">>" -> *)
    | "<" { LT }
    | ">" { GT }
    | "<=" { LTE }
    | ">=" { GTE }
    | "==" { EQ }
    | "!=" { NEQ }
    (* | "^" ->
    | "|" ->
    | "&&" ->
    | "||" ->
    | "?" -> *)
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "..." { ELLIPSIS }
    | "=" { ASSIGN }
    (* | "*=  " ->
    | "/=  " ->
    | "%=  " ->
    | "+=  " ->
    | "-=  " ->
    | "<<=  " ->
    | ">>=  " ->
    | "&= " ->
    | "^=  " ->
    | "|=" -> *)
    | "," { COMMA }
    (* | "#" ->
    | "##" -> *)
    | eof { EOF }

{}
