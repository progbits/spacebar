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
%token FOR
%token GOTO
%token IF
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
%token LBRACE "{"
%token RBRACE "}"
%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token DOT
%token STAR
%token PLUS
%token MINUS
%token SLASH
%token MOD
%token LT
%token GT
%token LTE
%token GTE
%token EQ
%token NEQ
%token COLON
%token SEMICOLON ";"
%token ELLIPSIS "..."
%token ASSIGN
%token COMMA ","

%token EOF

// Entrypoint.
%start <external_declaration list> translation_unit

%%

translation_unit:
    | list(external_declaration) EOF { $1 }

external_declaration:
    | function_definition { FunctionDefinition $1 }
    | declaration { Declaration $1 }

// 6.7.6 Declarators
declarator:
    | option(pointer) direct_declarator { 
        { pointer=$1; direct_declarator=$2 }
    }

direct_declarator:
    | IDENTIFIER { Ast.Identifier $1 }
    | "("; x = declarator; ")" { Ast.Declarator x }
    | x = direct_declarator; "("; y = parameter_type_list; ")" { 
        Ast.FunctionDeclarator { direct_declarator=x; parameter_list=y }
    }

pointer:
    | STAR; option(type_qualifier_list) {
        {type_qualifier_list=$2; pointer=None }
    }
    | STAR; option(type_qualifier_list); pointer { 
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

// 6.9.1 Function definitions.
function_definition:
    | declaration_specifiers declarator option(declaration_list) compound_statement { 
        { declaration_specifiers=$1
        ; declarator=$2
        ; declaration_list=$3
        ; compound_statement=None }
    }

declaration_list:
    | declaration { [$1] }
    | declaration_list declaration { $2 :: $1 }

primary_expression:
    | IDENTIFIER { }
    | CONSTANT { }
    | STRING_LITERAL { }
    | "(" expression ")" { }
    // | generic_selection { }

postfix_expression:
    | primary_expression { }
    | postfix_expression "[" expression "]" { }
    | postfix_expression "(" option(argument_expression_list) ")" { }
    | postfix_expression DOT IDENTIFIER { }

argument_expression_list:
    | assignment_expression { }
    | argument_expression_list "," assignment_expression { }

// 6.5.3 Unary Operators
unary_expression:
    | postfix_expression { }

// 6.7 Declarations
declaration:
    | declaration_specifiers; option(init_declarator_list); ";" { 
        { declaration_specifiers=$1; init_declarator_list=$2 }
    }

declaration_specifiers:
    | type_specifier option(declaration_specifiers) { 
        Ast.TypeSpecifier { type_specifier=$1; declaration_specifiers=$2 }
    }
    | type_qualifier option(declaration_specifiers) { 
        Ast.TypeQualifier { type_qualifier=$1; declaration_specifiers=$2 }
    }
    | function_specifier option(declaration_specifiers) { 
        Ast.FunctionSpecifier { function_specifier=$1; declaration_specifiers=$2 }
    }

init_declarator_list:
    | init_declarator { [$1] }
    | init_declarator_list; ","; init_declarator { $3 :: $1 }

init_declarator:
    | declarator { { declarator=$1; _initializer=None } }
    | declarator; ASSIGN; initializerr { 
        { declarator=$1; _initializer=None }
    }

type_specifier:
    | VOID { Void }
    | CHAR { Char }
    | INT { Int }
    | struct_or_union_specifier { $1 }

struct_or_union_specifier:
    | struct_or_union option(IDENTIFIER) "[" struct_declaration_list "]" { $1 }
    | struct_or_union IDENTIFIER { $1 }

struct_or_union:
    | STRUCT { Struct }
    | UNION { Union }

struct_declaration_list:
    | struct_declaration { }
    | struct_declaration_list struct_declaration { }

struct_declaration:
    | specifier_qualifier_list option(struct_declarator_list) ";" { }

specifier_qualifier_list:
    | type_specifier option(specifier_qualifier_list) { }
    | type_qualifier option(specifier_qualifier_list) { }

struct_declarator_list:
    | struct_declarator { }
    | struct_declarator_list "," struct_declarator { }

struct_declarator:
    | declarator { }
    | option(declarator) COLON constant_expression { }

type_qualifier:
    | CONST { Const }

function_specifier:
    | INLINE { Inline }
    | NORETURN { NoReturn }

// 6.5.17 Expression
expression:
    | assignment_expression { }
    | expression "," assignment_expression { }

// 6.6 Constant expressions
constant_expression:
    | conditional_expression { }

assignment_expression:
    | conditional_expression { }

conditional_expression:
    | logical_or_expression { }

logical_or_expression:
    | inclusive_or_experssion { }

inclusive_or_experssion:
    | exclusive_or_expression { }

exclusive_or_expression:
    | and_expression { }

and_expression:
    | equality_expression { }

equality_expression:
    | relational_expression { }
    | equality_expression EQ relational_expression { }
    | equality_expression NEQ relational_expression { }

relational_expression:
    | shift_expression { }
    | relational_expression LT shift_expression { }
    | relational_expression GT shift_expression { }
    | relational_expression LTE shift_expression { }
    | relational_expression GTE shift_expression { }

shift_expression:
    | additive_expression { }

additive_expression:
    | multiplicative_expression { }
    | additive_expression PLUS multiplicative_expression { }
    | additive_expression MINUS multiplicative_expression { }

multiplicative_expression:
    | cast_expression { }
    | multiplicative_expression STAR cast_expression { }
    | multiplicative_expression SLASH cast_expression { }
    | multiplicative_expression MOD cast_expression { }

cast_expression:
    | unary_expression { }

// 6.7.9 Initialization
initializerr:
    | assignment_expression { }
    | "{" initializer_list "}" { }
    | "{" initializer_list "," "}" { }

initializer_list:
    | option(designation) initializerr { }
    | initializer_list "," option(designation) initializerr { }

designation:
    | designator_list { }

designator_list:
    | designator { }
    | designator_list designator { }

designator:
    | "[" constant_expression "]" { }
    | DOT IDENTIFIER { }

// 6.8 Statements and blocks
statement:
    | labeled_statement { }
    | compound_statement { }
    | expression_statement { }
    | selection_statement { }
    | iteration_statement { }
    // | jump_statement { }

labeled_statement:
    | IDENTIFIER COLON statement { }
    | CASE constant_expression COLON statement { }
    | DEFAULT COLON statement { }

compound_statement:
    | "[" option(block_item_list) "]" { }

block_item_list:
    | block_item { }
    | block_item_list block_item { }

block_item:
    | declaration { }
    | statement { }

expression_statement:
    | option(expression) ";" { }

selection_statement:
    | IF "(" expression ")" statement { }
    | IF "(" expression ")" statement ELSE statement { }

iteration_statement:
    | WHILE "(" expression ")" statement { }
    | DO statement WHILE "(" expression ")" ";" { }
    | FOR "(" option(expression) ";" option(expression) ";" option(expression) ")" statement { }
    | FOR "(" declaration option(expression) ";" option(expression) ")" statement { }

;
