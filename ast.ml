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
  | Expression of expression
  | FunctionCallExpression of string

(* 6.5.2 *)
and postfix_expression =
  | PrimaryExpression of primary_expression
  | ArrayAccess of
      {postfix_expression: postfix_expression; expression: expression}
  | FunctionCall of
      {postfix_expression: postfix_expression; expression: expression}
  | MemberAccess of {postfix_expression: postfix_expression; identifier: string}
  | PointerMemberAccess of
      {postfix_expression: postfix_expression; identifier: string}
  | PostfixIncrement of {postfix_expression: postfix_expression}
  | PostfixDecrement of {postfix_expression: postfix_expression}

(* 6.5.3 *)
and unary_expression =
  | PostfixExpression of postfix_expression
  | PrefixIncrement of {unary_expression: unary_expression}
  | PrefixDecrement of {unary_expression: unary_expression}
  | UnaryOperator of
      {operator: unary_operator; unary_expression: unary_expression}

(* 6.5.3 *)
and unary_operator =
  | AddressOf of unit
  | PointeDereference of unit
  | UnaryPlus of unit
  | UnaryMinus of unit
  | UnaryBitwiseNot of unit
  | UnaryNot of unit

(* 6.5.5 *)
and multiplicative_expression =
  | CastExpression of unary_expression
  | MultiplicativeProduct of
      { multiplicative_expression: multiplicative_expression
      ; cast_expression: unary_expression }
  | MultiplicativeDivision of
      { multiplicative_expression: multiplicative_expression
      ; cast_expression: unary_expression }
  | MultiplicativeRemainder of
      { multiplicative_expression: multiplicative_expression
      ; cast_expression: unary_expression }

(* 6.5.6 *)
and additive_expression =
  | MultiplicativeExpression of multiplicative_expression
  | AdditiveAdditionExpression of
      { additive_expression: additive_expression
      ; multiplicative_expression: multiplicative_expression }
  | AdditiveSubtractionExpression of
      { additive_expression: additive_expression
      ; multiplicative_expression: multiplicative_expression }

(* 6.5.7 *)
and shift_expression =
  | AdditiveExpression of additive_expression
  | LeftShiftExpression of
      { shift_expression: shift_expression
      ; additive_expression: additive_expression }
  | RightShiftExpression of
      { shift_expression: shift_expression
      ; additive_expression: additive_expression }

(* 6.5.8 *)
and relational_expression =
  | ShiftExpression of shift_expression
  | LessThanExpression of
      { relational_expression: relational_expression
      ; shift_expression: shift_expression }
  | GreaterThanExpression of
      { relational_expression: relational_expression
      ; shift_expression: shift_expression }
  | LessThanEqualThanExpression of
      { relational_expression: relational_expression
      ; shift_expression: shift_expression }
  | GreaterThanEqualExpression of
      { relational_expression: relational_expression
      ; shift_expression: shift_expression }

(* 6.5.9 *)
and equality_expression =
  | RelationalExpression of relational_expression
  | EqualToExpression of
      { equality_expression: equality_expression
      ; relational_expression: relational_expression }
  | NotEqualToExpression of
      { equality_expression: equality_expression
      ; relational_expression: relational_expression }

(* 6.5.10 *)
and and_expression =
  | EqualityExpression of equality_expression
  | BitwiseAndExpression of
      {and_expression: and_expression; equality_expression: equality_expression}

(* 6.5.11 *)
and exclusive_or_expression =
  | AndExpression of and_expression
  | ExclusiveBitwiseOrExpression of
      { exclusive_or_expression: exclusive_or_expression
      ; and_expression: and_expression }

(* 6.5.12 *)
and inclusive_or_expression =
  | ExclusiveOr of exclusive_or_expression
  | InclusiveBitwiseOrExpression of
      { inclusive_or_expression: inclusive_or_expression
      ; exclusive_or_expression: exclusive_or_expression }

(* 6.5.13 *)
and logical_and_expression =
  | InclusiveOrExpression of inclusive_or_expression
  | LogicalAndExpression of
      { logical_and_expression: logical_and_expression
      ; inclusive_or_expression: inclusive_or_expression }

(* 6.5.14 *)
and logical_or_expression =
  | LogicalOrLogicalAndExpression of logical_and_expression
  | LogicalOrExpression of
      { logical_or_expression: logical_or_expression
      ; logical_and_expression: logical_and_expression }

(* 6.5.15 *)
and conditional_expression =
  | ContitionalLogicalOrExpression of logical_or_expression
  | ConditionalExpression of
      {a: logical_or_expression; b: expression; c: conditional_expression}

(* 6.5.16 *)
and assignment_expression =
  | AssignmentConditionalExpression of conditional_expression
  | AssignmentOperation of
      { unary_expression: unary_expression
      ; assignment_operator: string
      ; assignment_expression: assignment_expression }

(* 6.5.17 *)
and expression = AssignmentExpression of assignment_expression

let print_unimplemented = Printf.printf "implement this!\n"

let print_primary_expression (x : primary_expression) =
  match x with
  | Identifier x' -> Printf.printf "%s\n" x'
  | Constant x' -> Printf.printf "%d\n" x'
  | StringLiteral x' -> Printf.printf "%s\n" x'
  | Expression _ -> print_unimplemented
  | FunctionCallExpression _ -> print_unimplemented

let print_postfix_expression (x : postfix_expression) =
  match x with
  | PrimaryExpression x' -> print_primary_expression x'
  | ArrayAccess _ -> print_unimplemented
  | FunctionCall _ -> print_unimplemented
  | MemberAccess _ -> print_unimplemented
  | PointerMemberAccess _ -> print_unimplemented
  | PostfixIncrement _ -> print_unimplemented
  | PostfixDecrement _ -> print_unimplemented

let print_unary_expresssion (x : unary_expression) =
  match x with
  | PostfixExpression x' -> print_postfix_expression x'
  | PrefixIncrement _ -> print_unimplemented
  | PrefixDecrement _ -> print_unimplemented
  | UnaryOperator _ -> print_unimplemented

let print_multiplicative_expression (x : multiplicative_expression) =
  match x with
  | CastExpression x' -> print_unary_expresssion x'
  | MultiplicativeProduct _ -> print_unimplemented
  | MultiplicativeDivision _ -> print_unimplemented
  | MultiplicativeRemainder _ -> print_unimplemented

let print_additive_expression (x : additive_expression) =
  match x with
  | MultiplicativeExpression x' -> print_multiplicative_expression x'
  | AdditiveAdditionExpression _ -> print_unimplemented
  | AdditiveSubtractionExpression _ -> print_unimplemented

let print_shift_expression (x : shift_expression) =
  match x with
  | AdditiveExpression x' -> print_additive_expression x'
  | LeftShiftExpression _ -> print_unimplemented
  | RightShiftExpression _ -> print_unimplemented

let print_relational_expression (x : relational_expression) =
  match x with
  | ShiftExpression x' -> print_shift_expression x'
  | LessThanExpression _ -> print_unimplemented
  | GreaterThanExpression _ -> print_unimplemented
  | LessThanEqualThanExpression _ -> print_unimplemented
  | GreaterThanEqualExpression _ -> print_unimplemented

let print_equality_expression (x : equality_expression) =
  match x with
  | RelationalExpression x' -> print_relational_expression x'
  | EqualToExpression _ -> print_unimplemented
  | NotEqualToExpression _ -> print_unimplemented

let print_and_expression (x : and_expression) =
  match x with
  | EqualityExpression x' -> print_equality_expression x'
  | BitwiseAndExpression _ -> print_unimplemented

let print_exclusive_or_expression (x : exclusive_or_expression) =
  match x with
  | AndExpression x' -> print_and_expression x'
  | ExclusiveBitwiseOrExpression _ -> print_unimplemented

let print_inclusive_or_expression (x : inclusive_or_expression) =
  match x with
  | ExclusiveOr x' -> print_exclusive_or_expression x'
  | InclusiveBitwiseOrExpression _ -> print_unimplemented

let print_logical_and_expression (x : logical_and_expression) =
  match x with
  | InclusiveOrExpression x' -> print_inclusive_or_expression x'
  | LogicalAndExpression _ -> print_unimplemented

let print_logical_or_expression (x : logical_or_expression) =
  match x with
  | LogicalOrLogicalAndExpression x' -> print_logical_and_expression x'
  | LogicalOrExpression _ -> print_unimplemented

let print_conditional_expression (x : conditional_expression) =
  match x with
  | ContitionalLogicalOrExpression x' -> print_logical_or_expression x'
  | ConditionalExpression _ -> print_unimplemented

let print_assignment_expression (x : assignment_expression) =
  match x with
  | AssignmentConditionalExpression x' -> print_conditional_expression x'
  | AssignmentOperation _ -> print_unimplemented

(*let print_expression (x: expression) =
  match x with
  | AssignmentExpression x' -> print_assignment_expression x'*)

(* 6.7 *)
type init_declarator =
  {declarator: declarator; _initializer: assignment_expression option}

type declaration =
  { declaration_specifiers: declaration_specifiers
  ; init_declarator_list: init_declarator list option }

(* 6.8 *)
type statement =
  | LabeledStatement of string
  | CompoundStatement of block_item list option
  | ExpressionStatement of expression option
  | SelectionStatement of string
  | IterationStatement of string
  | JumpStatement of string

(* 6.8.1 *)
(*and labeled_statement = *)

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
  match x._initializer with
  | Some x' -> print_assignment_expression x'
  | None -> Printf.printf "No initializer\n"

(* todo: print assignment_expression *)

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
