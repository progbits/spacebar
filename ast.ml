(* 6.7.1 *)
type storage_class = TypeDef | Extern | Static | ThreadLocal | Auto | Register

(* 6.7.2 *)
type type_specifier = Void | Char | Int | Struct | Union

(* 6.7.3 *)
type type_qualifier = Const | Volatile

(* 6.7.4 *)
type function_specifier = Inline | NoReturn

(* 6.7.5 - Not implemented *)
type alignment_specifier = None

(* 6.7 *)
type declaration_specifiers =
  | StorageClass of
      { storage_class: storage_class
      ; declaration_specifiers: declaration_specifiers option }
  | TypeSpecifier of
      { type_specifier: type_specifier
      ; declaration_specifiers: declaration_specifiers option }
  | TypeQualifier of
      { type_qualifier: type_qualifier
      ; declaration_specifiers: declaration_specifiers option }
  | FunctionSpecifier of
      { function_specifier: function_specifier
      ; declaration_specifiers: declaration_specifiers option }
  | AlignmentSpecifier of
      { alignment_specifier: alignment_specifier
      ; declaration_specifiers: declaration_specifiers option }

(* 6.7.6 *)
type pointer =
  {type_qualifier_list: type_qualifier list option; pointer: pointer option}

(* 6.7.6 *)
type declarator = {pointer: pointer option; direct_declarator: direct_declarator}

(* 6.7.6 *)
and direct_declarator =
  | Identifier of string
  | Declarator of declarator
  | FunctionDeclarator of
      { direct_declarator: direct_declarator
      ; parameter_list: parameter_declaration list }

and parameter_declaration =
  {declaration_specifiers: declaration_specifiers; declarator: declarator}

(* 6.5.1 *)
type primary_expression =
  | Identifier of string
  | Constant of int
  | StringLiteral of string
  | Expression of string
  | FunctionCallExpression of string

(* 6.7 *)
type init_declarator =
  {declarator: declarator; _initializer: primary_expression option}

type declaration =
  { declaration_specifiers: declaration_specifiers
  ; init_declarator_list: init_declarator list option }

type statement =
  | LabeledStatement of string
  | CompoundStatement of block_item list option
  | ExpressionStatement of string
  | SelectionStatement of string
  | IterationStatement of string
  | JumpStatement of string

(* 6.8.2 *)
and block_item = Declaration of declaration | Statement of statement

(* 6.9.1 *)
type function_definition =
  { declaration_specifiers: declaration_specifiers
  ; declarator: declarator
  ; declaration_list: declaration list option
  ; compound_statement: block_item list option }

(* 6.9 *)
type external_declaration =
  | FunctionDefinition of function_definition
  | Declaration of declaration

(*

  AST pretty printer

*)

(* Pretty print `type_specifier` *)
let print_type_specifier t =
  match t with
  | Void -> Printf.printf "Void\n"
  | Char -> Printf.printf "Char\n"
  | Int -> Printf.printf "Int\n"
  | Struct -> Printf.printf "Struct\n"
  | Union -> Printf.printf "Union\n"

(* Pretty print `type_qualifier` *)
let print_type_qualifier q =
  match q with
  | Const -> Printf.printf "Const\n"
  | Volatile -> Printf.printf "Volatile\n"

(* Pretty print `declaration_specifiers` *)
let rec print_declaration_specifiers d =
  match d with
  | StorageClass x -> (
      Printf.printf "StorageClass\n" ;
      match x.declaration_specifiers with
      | Some d' -> print_declaration_specifiers d'
      | None -> () )
  | TypeSpecifier x -> (
      Printf.printf "TypeSpecifer\n" ;
      print_type_specifier x.type_specifier ;
      match x.declaration_specifiers with
      | Some d' -> print_declaration_specifiers d'
      | None -> () )
  | TypeQualifier x -> (
      Printf.printf "TypeQualifier\n" ;
      print_type_qualifier x.type_qualifier ;
      match x.declaration_specifiers with
      | Some d' -> print_declaration_specifiers d'
      | None -> () )
  | FunctionSpecifier x -> (
      Printf.printf "FunctionSpecifier\n" ;
      match x.declaration_specifiers with
      | Some d' -> print_declaration_specifiers d'
      | None -> () )
  | AlignmentSpecifier x -> (
      Printf.printf "AlignmentSpecifier\n" ;
      match x.declaration_specifiers with
      | Some d' -> print_declaration_specifiers d'
      | None -> () )

(* Pretty print `pointer` *)
let rec print_pointer p =
  Printf.printf "Pointer\n" ;
  let _ =
    match p.type_qualifier_list with
    | Some l -> List.iter print_type_qualifier l
    | None -> ()
  in
  let _ = match p.pointer with Some p -> print_pointer p | None -> () in
  ()

(* Pretty print `direct_declarator` *)
let rec print_direct_declarator (x : direct_declarator) =
  match x with
  | Identifier x' -> Printf.printf "%s\n" x'
  | FunctionDeclarator x' ->
      Printf.printf "FunctionDeclarator\n" ;
      print_direct_declarator x'.direct_declarator ;
      List.iter print_parameter_declaration x'.parameter_list
  | _ -> Printf.printf "another declarator\n"

(* Pretty print `declarator` *)
and print_declarator x =
  Printf.printf "Declarator\n" ;
  let _ = match x.pointer with Some x' -> print_pointer x' | None -> () in
  print_direct_declarator x.direct_declarator

(* Pretty print `parameter_list` *)
and print_parameter_declaration x =
  print_declaration_specifiers x.declaration_specifiers ;
  print_declarator x.declarator

(* Pretty print `init_declarator` *)
and print_init_declarator (x : init_declarator) =
  Printf.printf "InitDeclarator\n" ;
  print_declarator x.declarator ;
  let _ =
    match x._initializer with
    | Some x' -> (
      match x' with
      | Identifier x'' -> Printf.printf "Identifier: %s\n" x''
      | Constant x'' -> Printf.printf "Constant: %d\n" x''
      | StringLiteral x'' -> Printf.printf "StringLiteral: %s\n" x''
      | Expression x'' -> Printf.printf "Expression: %s\n" x''
      | FunctionCallExpression x'' ->
          Printf.printf "FunctionCallExpression: %s\n" x'' )
    | None -> Printf.printf "No initializer!\n"
  in
  ()

(* Pretty print `declaration` *)
let print_declaration (x : declaration) =
  print_declaration_specifiers x.declaration_specifiers ;
  match x.init_declarator_list with
  | Some x' -> List.iter print_init_declarator x'
  | None -> Printf.printf "No init_declarator_list\n"

(* Pretty print `block_item` *)
let print_block_item (x : block_item) =
  match x with
  | Declaration x' -> print_declaration x'
  | Statement _ -> Printf.printf "statement\n"

(* Pretty print `external_declaration` *)
let print_external_declaration x =
  Printf.printf "ExternalDeclaration\n" ;
  match x with
  | FunctionDefinition x' ->
      Printf.printf "FunctionDefinition\n" ;
      print_declaration_specifiers x'.declaration_specifiers ;
      print_declarator x'.declarator ;
      let _ =
        match x'.declaration_list with
        | Some x'' -> List.iter print_declaration x''
        | None -> ()
      in
      let _ =
        match x'.compound_statement with
        | Some x'' -> List.iter print_block_item x''
        | None -> Printf.printf "Empty compound_statement"
      in
      ()
  | Declaration x' -> print_declaration x'

(* Pretty print `translation_unit *)
let print_translation_unit x = List.iter print_external_declaration x
