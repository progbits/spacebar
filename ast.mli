type storage_class = TypeDef | Extern | Static | ThreadLocal | Auto | Register

type type_specifier = Void | Char | Int | Struct | Union

type type_qualifier = Const | Volatile

type function_specifier = Inline | NoReturn

type alignment_specifier = None

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

type init_declarator = {declarator: declarator; _initializer: string option}

type declaration =
  { declaration_specifiers: declaration_specifiers
  ; init_declarator_list: init_declarator list option }

type block_item = Declaration of declaration | Statement of string

type function_definition =
  { declaration_specifiers: declaration_specifiers
  ; declarator: declarator
  ; declaration_list: declaration list option
  ; compound_statement: block_item list option }

type external_declaration =
  | FunctionDefinition of function_definition
  | Declaration of declaration

val print_type_specifier : type_specifier -> unit

val print_type_qualifier : type_qualifier -> unit

val print_declaration_specifiers : declaration_specifiers -> unit

val print_pointer : pointer -> unit

val print_direct_declarator : direct_declarator -> unit

val print_declarator : declarator -> unit

val print_parameter_declaration : parameter_declaration -> unit

val print_init_declarator : init_declarator -> unit

val print_declaration : declaration -> unit

val print_external_declaration : external_declaration -> unit

val print_translation_unit : external_declaration list -> unit
