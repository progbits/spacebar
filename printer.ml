open Ast

let print_primary_expression (x : primary_expression) =
  match x with
  | Identifier x' -> Printf.printf "%s\n" x'
  | Constant x' -> Printf.printf "%d\n" x'
  | StringLiteral x' -> Printf.printf "%s\n" x'
  | Expression _ -> Printf.printf "Expression\n"
  | FunctionCallExpression _ -> Printf.printf "FunctionCallExpression\n"

let rec print_postfix_expression (x : postfix_expression) =
  match x with
  | PrimaryExpression x' -> print_primary_expression x'
  | ArrayAccess x' ->
      print_postfix_expression x'.postfix_expression
      (*print_expression x'.expression*)
  | FunctionCall _ -> Printf.printf "FunctionCall\n"
  | MemberAccess _ -> Printf.printf "MemberAccess\n"
  | PointerMemberAccess _ -> Printf.printf "PointerMemberAccess\n"
  | PostfixIncrement _ -> Printf.printf "PostfixIncrement\n"
  | PostfixDecrement _ -> Printf.printf "PostfixDecrement\n"

let print_unary_expresssion (x : unary_expression) =
  match x with
  | PostfixExpression x' -> print_postfix_expression x'
  | PrefixIncrement _ -> Printf.printf "PrefixIncrement\n"
  | PrefixDecrement _ -> Printf.printf "PrefixDecrement\n"
  | UnaryOperator _ -> Printf.printf "UnaryOperator\n"

let rec print_multiplicative_expression (x : multiplicative_expression) =
  match x with
  | CastExpression x' -> print_unary_expresssion x'
  | MultiplicativeProduct x' ->
      Printf.printf "MultiplicativeProduct\n" ;
      print_multiplicative_expression x'.multiplicative_expression ;
      Printf.printf "*\n" ;
      print_unary_expresssion x'.cast_expression
  | MultiplicativeDivision x' ->
      Printf.printf "MultiplicativeDivision\n" ;
      print_multiplicative_expression x'.multiplicative_expression ;
      Printf.printf "/\n" ;
      print_unary_expresssion x'.cast_expression
  | MultiplicativeRemainder x' ->
      Printf.printf "MultiplicativeRemainder\n" ;
      print_multiplicative_expression x'.multiplicative_expression ;
      Printf.printf "%%\n" ;
      print_unary_expresssion x'.cast_expression

let rec print_additive_expression (x : additive_expression) =
  match x with
  | MultiplicativeExpression x' -> print_multiplicative_expression x'
  | AdditiveAdditionExpression x' ->
      Printf.printf "AdditiveAdditionExpression\n" ;
      print_additive_expression x'.additive_expression ;
      Printf.printf "+\n" ;
      print_multiplicative_expression x'.multiplicative_expression
  | AdditiveSubtractionExpression _ ->
      Printf.printf "AdditiveSubtractionExpression\n"

let print_shift_expression (x : shift_expression) =
  match x with
  | AdditiveExpression x' -> print_additive_expression x'
  | LeftShiftExpression _ -> Printf.printf "LeftShiftExpression\n"
  | RightShiftExpression _ -> Printf.printf "RightShiftExpression\n"

let print_relational_expression (x : relational_expression) =
  match x with
  | ShiftExpression x' -> print_shift_expression x'
  | LessThanExpression _ -> Printf.printf "LessThanExpression\n"
  | GreaterThanExpression _ -> Printf.printf "GreaterThanExpression\n"
  | LessThanEqualThanExpression _ ->
      Printf.printf "LessThanEqualThanExpression\n"
  | GreaterThanEqualExpression _ -> Printf.printf "GreaterThanEqualExpression\n"

let print_equality_expression (x : equality_expression) =
  match x with
  | RelationalExpression x' -> print_relational_expression x'
  | EqualToExpression _ -> Printf.printf "EqualToExpression\n"
  | NotEqualToExpression _ -> Printf.printf "NotEqualToExpression\n"

let print_and_expression (x : and_expression) =
  match x with
  | EqualityExpression x' -> print_equality_expression x'
  | BitwiseAndExpression _ -> Printf.printf "BitwiseAndExpression\n"

let print_exclusive_or_expression (x : exclusive_or_expression) =
  match x with
  | AndExpression x' -> print_and_expression x'
  | ExclusiveBitwiseOrExpression _ ->
      Printf.printf "ExclusiveBitwiseOrExpression\n"

let print_inclusive_or_expression (x : inclusive_or_expression) =
  match x with
  | ExclusiveOr x' -> print_exclusive_or_expression x'
  | InclusiveBitwiseOrExpression _ ->
      Printf.printf "InclusiveBitwiseOrExpression\n"

let print_logical_and_expression (x : logical_and_expression) =
  match x with
  | InclusiveOrExpression x' -> print_inclusive_or_expression x'
  | LogicalAndExpression _ -> Printf.printf "LogicalAndExpression\n"

let print_logical_or_expression (x : logical_or_expression) =
  match x with
  | LogicalOrLogicalAndExpression x' -> print_logical_and_expression x'
  | LogicalOrExpression _ -> Printf.printf "LogicalOrExpression\n"

let print_conditional_expression (x : conditional_expression) =
  match x with
  | ContitionalLogicalOrExpression x' -> print_logical_or_expression x'
  | ConditionalExpression _ -> Printf.printf "ConditionalExpression\n"

let rec print_assignment_expression (x : assignment_expression) =
  Printf.printf "Assignment Expression\n" ;
  match x with
  | AssignmentConditionalExpression x' ->
      Printf.printf "conditional!!!\n" ;
      print_conditional_expression x'
  | AssignmentOperation x' ->
      print_unary_expresssion x'.unary_expression ;
      Printf.printf "Assignment operator\n" ;
      print_assignment_expression x'.assignment_expression

(*let print_expression (x: expression) =
  match x with
  | AssignmentExpression x' -> print_assignment_expression x':*)

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
