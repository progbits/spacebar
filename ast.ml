(* 6.7.1 *)
type storage_class = TypeDef | Extern | Static | ThreadLocal | Auto | Register

(* 6.7.2 *)
type type_specifier = Void | Char | Int | Struct | Union

(* 6.7.3 *)
type type_qualifier = Const | Volatile

(* 6.7.4 *)
type function_specifier = Inline | NoReturn

(* 6.7.5 - Not implemented *)
type alignment_specifier = Alignas

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

(* 6.5.2 *)
and postfix_expression =
  | PrimaryExpression of primary_expression
  | ArrayAccess of
      {postfix_expression: postfix_expression; expression: expression}
  | FunctionCall of
      { postfix_expression: postfix_expression
      ; argument_expression_list: assignment_expression list }
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
  | PointerDereference of unit
  | UnaryPlus of unit
  | UnaryMinus of unit
  | UnaryBitwiseNot of unit
  | UnaryNot of unit

and assignment_operator = Assign of unit

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
      ; assignment_operator: assignment_operator
      ; assignment_expression: assignment_expression }

(* 6.5.17 *)
and expression = AssignmentExpression of assignment_expression

(* 6.7 *)
type init_declarator =
  {declarator: declarator; _initializer: assignment_expression option}

type declaration =
  { declaration_specifiers: declaration_specifiers
  ; init_declarator_list: init_declarator list }

(* 6.8 *)
type statement =
  | LabeledStatement of string
  | CompoundStatement of block_item list
  | ExpressionStatement of expression option
  | SelectionStatement of selection_statement
  | IterationStatement of iteration_statement
  | JumpStatement of jump_statement

(* 6.8.1 *)
(*and labeled_statement = *)

(* 6.8.2 *)
and block_item = Declaration of declaration | Statement of statement

and selection_statement =
  | If of {expression: expression; body: statement}
  | IfElse of {expression: expression; body: statement; else_body: statement}
  | Switch of {expression: expression; body: statement}

and iteration_statement =
  | While of {expression: expression; body: statement}
  | DoWhile of {body: statement; expression: expression}
  | For of
      { init_decl: declaration option
      ; init_expr: expression option
      ; condition: expression option
      ; iteration: expression option
      ; body: statement }

and jump_statement =
  | Goto of string
  | Continue
  | Break
  | Return of {expression: expression option}

(* 6.9.1 *)
type function_definition =
  { declaration_specifiers: declaration_specifiers
  ; declarator: declarator
  ; declaration_list: declaration list option
  ; compound_statement: block_item list }

(* 6.9 *)
type external_declaration =
  | FunctionDefinition of function_definition
  | Declaration of declaration
