type storage_class = TypeDef | Extern | Static | ThreadLocal | Auto | Register

type type_specifier = Void | Char | Int | Struct | Union

type type_qualifier = Const | Volatile

type function_specifier = Inline | NoReturn

type alignment_specifier = Alignas

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

type pointer =
  {type_qualifier_list: type_qualifier list option; pointer: pointer option}

type declarator = {pointer: pointer option; direct_declarator: direct_declarator}

and direct_declarator =
  | Identifier of string
  | Declarator of declarator
  | FunctionDeclarator of
      { direct_declarator: direct_declarator
      ; parameter_list: parameter_declaration list }

and parameter_declaration =
  {declaration_specifiers: declaration_specifiers; declarator: declarator}

type primary_expression =
  | Identifier of string
  | Constant of int
  | StringLiteral of string
  | Expression of expression

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

and unary_expression =
  | PostfixExpression of postfix_expression
  | PrefixIncrement of {unary_expression: unary_expression}
  | PrefixDecrement of {unary_expression: unary_expression}
  | UnaryOperator of
      {operator: unary_operator; unary_expression: unary_expression}

and unary_operator =
  | AddressOf of unit
  | PointeDereference of unit
  | UnaryPlus of unit
  | UnaryMinus of unit
  | UnaryBitwiseNot of unit
  | UnaryNot of unit

and assignment_operator = Assign of unit

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

and additive_expression =
  | MultiplicativeExpression of multiplicative_expression
  | AdditiveAdditionExpression of
      { additive_expression: additive_expression
      ; multiplicative_expression: multiplicative_expression }
  | AdditiveSubtractionExpression of
      { additive_expression: additive_expression
      ; multiplicative_expression: multiplicative_expression }

and shift_expression =
  | AdditiveExpression of additive_expression
  | LeftShiftExpression of
      { shift_expression: shift_expression
      ; additive_expression: additive_expression }
  | RightShiftExpression of
      { shift_expression: shift_expression
      ; additive_expression: additive_expression }

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

and equality_expression =
  | RelationalExpression of relational_expression
  | EqualToExpression of
      { equality_expression: equality_expression
      ; relational_expression: relational_expression }
  | NotEqualToExpression of
      { equality_expression: equality_expression
      ; relational_expression: relational_expression }

and and_expression =
  | EqualityExpression of equality_expression
  | BitwiseAndExpression of
      {and_expression: and_expression; equality_expression: equality_expression}

and exclusive_or_expression =
  | AndExpression of and_expression
  | ExclusiveBitwiseOrExpression of
      { exclusive_or_expression: exclusive_or_expression
      ; and_expression: and_expression }

and inclusive_or_expression =
  | ExclusiveOr of exclusive_or_expression
  | InclusiveBitwiseOrExpression of
      { inclusive_or_expression: inclusive_or_expression
      ; exclusive_or_expression: exclusive_or_expression }

and logical_and_expression =
  | InclusiveOrExpression of inclusive_or_expression
  | LogicalAndExpression of
      { logical_and_expression: logical_and_expression
      ; inclusive_or_expression: inclusive_or_expression }

and logical_or_expression =
  | LogicalOrLogicalAndExpression of logical_and_expression
  | LogicalOrExpression of
      { logical_or_expression: logical_or_expression
      ; logical_and_expression: logical_and_expression }

and conditional_expression =
  | ContitionalLogicalOrExpression of logical_or_expression
  | ConditionalExpression of
      {a: logical_or_expression; b: expression; c: conditional_expression}

and assignment_expression =
  | AssignmentConditionalExpression of conditional_expression
  | AssignmentOperation of
      { unary_expression: unary_expression
      ; assignment_operator: assignment_operator
      ; assignment_expression: assignment_expression }

and expression = AssignmentExpression of assignment_expression

type init_declarator =
  {declarator: declarator; _initializer: assignment_expression option}

type declaration =
  { declaration_specifiers: declaration_specifiers
  ; init_declarator_list: init_declarator list option }

type statement =
  | LabeledStatement of string
  | CompoundStatement of block_item list
  | ExpressionStatement of expression option
  | SelectionStatement of selection_statement
  | IterationStatement of iteration_statement
  | JumpStatement of string

and block_item = Declaration of declaration | Statement of statement

and selection_statement =
  | If of {expression: expression; body: statement}
  | IfElse of {expression: expression; body: statement; else_body: statement}
  | Switch of {expression: expression; body: statement}

and iteration_statement =
  | While of {expression: expression; body: statement}
  | DoWhile of {body: statement; expression: expression}
  | For of
      { init: expression option
      ; condition: expression option
      ; iteration: expression option
      ; body: statement }

type function_definition =
  { declaration_specifiers: declaration_specifiers
  ; declarator: declarator
  ; declaration_list: declaration list option
  ; compound_statement: block_item list }

type external_declaration =
  | FunctionDefinition of function_definition
  | Declaration of declaration
