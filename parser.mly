%{
    open Ast
%}

// Keywords.
%token ALIGNOF
%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR "for"
%token GOTO
%token IF "if"
%token INLINE
%token INT
%token LONG
%token REGISTER
%token RESTRICT
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE
%token ALIGNAS
%token ATOMIC
%token BOOL
%token COMPLEX
%token GENERIC
%token IMAGINARY
%token NORETURN
%token STATIC_ASSERT
%token THREAD_LOCAL

// Other tokens.
%token <string> IDENTIFIER
%token <int> CONSTANT
%token <string> STRING_LITERAL

// Punctuators.
%token LBRACKET "["
%token RBRACKET "]"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token DOT "."
%token AMPERSAND "&"
%token STAR "*"
%token PLUS "+"
%token MINUS "-"
%token TILDE "~"
%token BANG "!"
%token SLASH "/"
%token MOD "%"
%token LSHIFT "<<"
%token RSHIFT ">>"
%token LT "<"
%token GT ">"
%token LTE "<="
%token GTE ">="
%token EQ "=="
%token NEQ "!="
%token HAT "^"
%token PIPE "|"
%token AND "&&"
%token OR "||"
%token CONDITIONAL "?"
%token COLON ":"
%token SEMICOLON ";"
%token ELLIPSIS "..."
%token ASSIGN "="
%token COMMA ","

%token EOF

%left "(" ")" "[" "]" "."
%left "*" "/" "%"
%left "+" "-"
%left ">" ">=" "<" "<="
%left "==" "!="
%right "="
%left ","

// Entrypoint.
%start <external_declaration list> translation_unit

%%

translation_unit:
    | list(external_declaration) EOF { $1 }

external_declaration:
    | function_definition { FunctionDefinition $1 }
    | declaration { Declaration $1 }

(* 6.7.6 *)
declarator:
    | pointer? direct_declarator {
        { pointer=$1; direct_declarator=$2 }
    }

direct_declarator:
    | IDENTIFIER { Ast.Identifier $1 }
    | "("; x = declarator; ")" { Ast.Declarator x }
    | x = direct_declarator; "("; y = parameter_type_list; ")" { 
        Ast.FunctionDeclarator { direct_declarator=x; parameter_list=y }
    }
    | x = direct_declarator; "("; identifier_list?; ")" {
        Ast.FunctionDeclarator { direct_declarator=x; parameter_list=[] }
    }

pointer:
    | STAR; type_qualifier_list? {
        {type_qualifier_list=$2; pointer=None }
    }
    | STAR; type_qualifier_list?; pointer {
        {type_qualifier_list=$2; pointer=Some $3 }
    }

type_qualifier_list:
    | type_qualifier { [$1] }
    | type_qualifier_list type_qualifier { $2 :: $1 }

parameter_type_list:
    | parameter_list { $1 }
    // | parameter_list "," "..." { }

parameter_list:
    | paramater_declaration { [$1] }
    | x = parameter_list; ","; y = paramater_declaration { y :: x }

paramater_declaration:
    | declaration_specifiers declarator {
        { declaration_specifiers=$1; declarator=$2 }
    }

identifier_list:
    | IDENTIFIER { [$1] }
    | identifier_list; ","; i = IDENTIFIER { i :: $1 }

(* 6.9.1 *)
function_definition:
    | declaration_specifiers declarator declaration_list? compound_statement {
        { declaration_specifiers=$1
        ; declarator=$2
        ; declaration_list=$3
        ; compound_statement=$4 }
    }

declaration_list:
    | declaration { [$1] }
    | declaration_list declaration { $2 :: $1 }

primary_expression:
    | IDENTIFIER { Ast.Identifier $1 }
    | CONSTANT { Ast.Constant $1 }
    | STRING_LITERAL { Ast.StringLiteral $1 }
    | "("; x = expression; ")" { Ast.Expression x }
    // | generic_selection { }

postfix_expression:
    | primary_expression { Ast.PrimaryExpression $1 }
    | x = postfix_expression; "["; y = expression; "]" {
        Ast.ArrayAccess {postfix_expression=x; expression=y}
    }
    | x = postfix_expression; "("; y = argument_expression_list?; ")" {
        match y with
        | Some y' -> Ast.FunctionCall { postfix_expression=x; argument_expression_list=y' }
        | None -> Ast.FunctionCall { postfix_expression=x; argument_expression_list=[] }
    }
    // | postfix_expression DOT IDENTIFIER { }

argument_expression_list:
    | assignment_expression { [$1] }
    | x = argument_expression_list; ","; y = assignment_expression { y :: x }

(* 6.5.3 *)
unary_expression:
    | postfix_expression { Ast.PostfixExpression $1 }
    | unary_operator cast_expression {
      Ast.UnaryOperator {operator=$1; unary_expression=$2 }
    }

(* 6.7 *)
declaration:
    | declaration_specifiers; init_declarator_list; ";" {
        { declaration_specifiers=$1; init_declarator_list=$2 }
    }

declaration_specifiers:
    | type_specifier declaration_specifiers? {
        Ast.TypeSpecifier { type_specifier=$1; declaration_specifiers=$2 }
    }
    | type_qualifier declaration_specifiers? {
        Ast.TypeQualifier { type_qualifier=$1; declaration_specifiers=$2 }
    }
    | function_specifier declaration_specifiers? {
        Ast.FunctionSpecifier { function_specifier=$1; declaration_specifiers=$2 }
    }

init_declarator_list:
    | init_declarator { [$1] }
    | init_declarator_list; ","; init_declarator { $3 :: $1 }

init_declarator:
    | declarator { { declarator=$1; _initializer=None } }
    | x = declarator; "="; y = initializerr { 
        { declarator=x; _initializer=Some y }
    }

type_specifier:
    | VOID { Void }
    | CHAR { Char }
    | INT { Int }
    | struct_or_union_specifier { $1 }

struct_or_union_specifier:
    | struct_or_union IDENTIFIER? "{" struct_declaration_list "}" { $1 }
    | struct_or_union IDENTIFIER { $1 }

struct_or_union:
    | STRUCT { Struct }
    | UNION { Union }

struct_declaration_list:
    | struct_declaration { }
    | struct_declaration_list struct_declaration { }

struct_declaration:
    | specifier_qualifier_list struct_declarator_list? ";" { }

specifier_qualifier_list:
    | type_specifier specifier_qualifier_list? { }
    | type_qualifier specifier_qualifier_list? { }

struct_declarator_list:
    | struct_declarator { }
    | struct_declarator_list "," struct_declarator { }

struct_declarator:
    | declarator { }
    | declarator? COLON constant_expression { }

type_qualifier:
    | CONST { Const }

function_specifier:
    | INLINE { Inline }
    | NORETURN { NoReturn }

(* 6.6 *)
constant_expression:
    | conditional_expression { }

(* 6.5.17 *)
expression:
    | assignment_expression { Ast.AssignmentExpression $1 }
    // | expression "," assignment_expression { }

(* 6.5.16 *)
assignment_operator:
    | "=" { Ast.Assign () }

(* 6.5.16 *)
assignment_expression:
    | conditional_expression { Ast.AssignmentConditionalExpression $1 }
    | unary_expression; assignment_operator; assignment_expression {
      Ast.AssignmentOperation {unary_expression=$1;assignment_operator=$2; assignment_expression=$3}
    }

(* 6.5.15 *)
conditional_expression:
    | logical_or_expression { Ast.ContitionalLogicalOrExpression $1 }
    | x = logical_or_expression; "?"; y = expression; ":"; z = conditional_expression {
      Ast.ConditionalExpression {a=x;b=y;c=z}
    }

(* 6.5.14 *)
logical_or_expression:
    | logical_and_expression { Ast.LogicalOrLogicalAndExpression $1 }
    // | logical_or_expression "||" logical_and_expression { }

(* 6.5.13 *)
logical_and_expression:
    | inclusive_or_experssion { Ast.InclusiveOrExpression $1 }
    // | logical_and_expression "&&" inclusive_or_experssion { }

(* 6.5.12 *)
inclusive_or_experssion:
    | exclusive_or_expression { Ast.ExclusiveOr $1 }
    | x = inclusive_or_experssion; "|"; y = exclusive_or_expression {
      Ast.InclusiveBitwiseOrExpression { inclusive_or_expression=x; exclusive_or_expression=y }
    }

(* 6.5.11 *)
exclusive_or_expression:
    | and_expression { Ast.AndExpression $1 }
    | x = exclusive_or_expression; "^"; y = and_expression {
      Ast.ExclusiveBitwiseOrExpression {exclusive_or_expression=x; and_expression=y}
    }

(* 6.5.10 *)
and_expression:
    | equality_expression { Ast.EqualityExpression $1 }
    | x = and_expression; "&"; y = equality_expression {
      Ast.BitwiseAndExpression { and_expression=x; equality_expression=y }
    }

(* 6.5.9 *)
equality_expression:
    | relational_expression { Ast.RelationalExpression $1 }
    | x = equality_expression; "=="; y = relational_expression {
      Ast.EqualToExpression { equality_expression=x; relational_expression=y }
    }
    | x = equality_expression; "!="; y = relational_expression {
      Ast.EqualToExpression { equality_expression=x; relational_expression=y }
    }

(* 6.5.8 *)
relational_expression:
    | shift_expression { Ast.ShiftExpression $1 }
    | x = relational_expression; "<"; y = shift_expression {
      Ast.LessThanExpression {relational_expression=x; shift_expression=y}
    }
    | x = relational_expression; ">"; y = shift_expression {
      Ast.GreaterThanExpression {relational_expression=x; shift_expression=y}
    }
    | x = relational_expression; "<="; y = shift_expression {
      Ast.LessThanEqualThanExpression {relational_expression=x; shift_expression=y}
    }
    | x = relational_expression; ">="; y = shift_expression {
      Ast.GreaterThanEqualExpression {relational_expression=x; shift_expression=y}
    }

(* 6.5.8 *)
shift_expression:
    | additive_expression { Ast.AdditiveExpression $1 }
    | x = shift_expression; "<<"; y = additive_expression {
      Ast.LeftShiftExpression {shift_expression=x; additive_expression=y}
    }
    | x = shift_expression; ">>"; y = additive_expression {
      Ast.RightShiftExpression {shift_expression=x; additive_expression=y}
    }

(* 6.5.6 *)
additive_expression:
    | multiplicative_expression { Ast.MultiplicativeExpression $1 }
    | x = additive_expression; "+"; y = multiplicative_expression {
      Ast.AdditiveAdditionExpression {additive_expression=x; multiplicative_expression=y}
    }
    | x = additive_expression; "-"; y = multiplicative_expression {
      Ast.AdditiveSubtractionExpression {additive_expression=x; multiplicative_expression=y}
    }

(* 6.5.5 *)
multiplicative_expression:
    | cast_expression { Ast.CastExpression $1 }
    | x = multiplicative_expression; "*"; y = cast_expression {
      Ast.MultiplicativeProduct {multiplicative_expression=x; cast_expression=y}
    }
    | x = multiplicative_expression; "/"; y = cast_expression {
      Ast.MultiplicativeDivision {multiplicative_expression=x; cast_expression=y}
    }
    | x = multiplicative_expression; "%"; y = cast_expression {
      Ast.MultiplicativeRemainder {multiplicative_expression=x; cast_expression=y}
    }

(* 6.5.4 *)
cast_expression:
    | unary_expression { $1 }
    // | "(" type_name ")" cast_expression { }

(* 6.5.3 *)
unary_operator:
    | "&" { AddressOf () }
    | "*" { PointerDereference () }
    | "+" { UnaryPlus () }
    | "-" { UnaryMinus () }
    | "~" { UnaryBitwiseNot () }
    | "!" { UnaryNot () }

(* 6.7.9 *)
initializerr:
    | assignment_expression { $1 }
    // | "{" initializer_list "}" { }
    // | "{" initializer_list "," "}" { }

initializer_list:
    | designation? initializerr { }
    | initializer_list "," designation? initializerr { }

designation:
    | designator_list { }

designator_list:
    | designator { }
    | designator_list designator { }

designator:
    | "[" constant_expression "]" { }
    | DOT IDENTIFIER { }

(* 6.8 *)
statement:
    | labeled_statement { Ast.LabeledStatement "" }
    | compound_statement { Ast.CompoundStatement $1 }
    | expression_statement { Ast.ExpressionStatement $1 }
    | selection_statement { Ast.SelectionStatement $1 }
    | iteration_statement { Ast.IterationStatement $1 }
    | jump_statement { Ast.JumpStatement $1 }

labeled_statement:
    | IDENTIFIER COLON statement { }
    | CASE constant_expression COLON statement { }
    | DEFAULT COLON statement { }

compound_statement:
    | "{"; x = block_item_list?; "}" {
      match x with
      | Some x' -> List.rev x'
      | None -> []
    }

block_item_list:
    | block_item { [$1] }
    | block_item_list block_item { $2 :: $1 }

block_item:
    | declaration { Ast.Declaration $1 }
    | statement { Ast.Statement $1 }

expression_statement:
    | expression? ";" { $1 }

selection_statement:
    | IF; "("; x = expression; ")"; y = statement {
      Ast.If {expression=x; body=y}
    }
    | IF; "("; x = expression; ")"; y = statement; ELSE; z = statement {
      Ast.IfElse {expression=x; body=y; else_body = z}
    }

iteration_statement:
    | WHILE; "("; x = expression; ")"; y = statement {
      Ast.While {expression=x; body=y}
    }
    | DO; x = statement; WHILE; "("; y = expression; ")"; ";" {
      Ast.DoWhile {body=x; expression=y}
    }
    | FOR; "("; x = expression?; ";"; y = expression?; ";"; z = expression?; ")"; w = statement {
      Ast.For {init=x; condition=y; iteration=z; body=w}
    }
    (*| FOR "(" declaration option(expression) ";" option(expression) ")" statement { }*)

jump_statement:
    | GOTO; id = IDENTIFIER { Ast.Goto id }
    | CONTINUE { Ast.Continue }
    | BREAK { Ast.Break }
    | RETURN; x = expression? { Ast.Return {expression=x} }

;
