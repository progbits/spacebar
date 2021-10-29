open Ast
open Whitespace

exception Duplicate_Function_Definition

exception Symbol_Not_Found

exception Function_Not_Found

(* An entry in the symbol table. *)
type symbol =
  | Global of {name: string; offset: int}
  | Function of {name: string; label: string}
  | Argument of {function_name: string; name: string; offset: int}
  | Variable of {function_name: string; name: string; offset: int}

(* The current state of the code generator. *)
type state = {ops: imp list; symbol_table: symbol list}

(* Returns the next function label. Functions are labelled incrementally
   starting from '1'. The label '0' is reserved for 'main'. *)
let next_label state =
  (* Helper. Count the symbol table entries of type Function. *)
  let rec count_fn_entries symbol_table i =
    match symbol_table with
    | [] -> i
    | h :: t -> (
      match h with
      | Function _ -> count_fn_entries t (i + 1)
      | _ -> count_fn_entries t i )
  in
  (* Label is the number of function entries in the table. *)
  string_of_int (count_fn_entries state.symbol_table 0)

(* Add a function to the symbol table and return the function label. Raises an
   exception of type Duplicate_Function_Definition if this symbol is already
   defined. *)
let add_fn state fn_name =
  (* Helper. Find a named entry of type Function. *)
  let rec fn_exists symbol_table name =
    match symbol_table with
    | [] -> false
    | h :: t -> (
      match h with
      | Function x when x.name = name -> true
      | _ -> fn_exists t name )
  in
  (* Add a new entry to the symbol table. *)
  match fn_exists state.symbol_table fn_name with
  | true ->
      (* Bit hacky, but we define `main` in our preamble, so we expect a
         duplicate definition when we actually get to the code. *)
      if fn_name = "main" then (state, "0")
      else raise Duplicate_Function_Definition
  | false ->
      let label = next_label state in
      let new_entry = Function {name= fn_name; label} in
      ({state with symbol_table= new_entry :: state.symbol_table}, label)

(* Return the label bound to a function. *)
let find_fn_label state fn_name =
  let rec find_label symbol_table fn_name =
    match symbol_table with
    | [] -> raise Function_Not_Found
    | h :: t -> (
      match h with
      | Function x when x.name = fn_name -> x.label
      | _ -> find_label t fn_name )
  in
  find_label state.symbol_table fn_name

(* Add a function argument to the symbol table and return its relative offset. *)
let add_fn_arg state fn_name arg_name =
  (* Count the number of arguments this function has. *)
  let rec count_fn_args symbol_table fn_name i =
    match symbol_table with
    | [] -> i
    | h :: t -> (
      match h with
      | Function x when x.name = fn_name -> count_fn_args t fn_name (i + 1)
      | _ -> count_fn_args t fn_name i )
  in
  let offset = count_fn_args state.symbol_table fn_name 0 in
  let new_entry = Argument {function_name= fn_name; name= arg_name; offset} in
  ({state with symbol_table= new_entry :: state.symbol_table}, offset)

(* Add a local variable to the symbol table and return its relative offset. *)
let add_local_var state fn_name var_name =
  (* Count the number of local variables this function has. *)
  let rec count symbol_table fn_name i =
    match symbol_table with
    | [] -> i + 1
    | h :: t -> (
      match h with
      | Variable x when x.function_name = fn_name -> count t fn_name (i + 1)
      | _ -> count t fn_name i )
  in
  let offset = count state.symbol_table fn_name 0 in
  let new_entry = Variable {function_name= fn_name; name= var_name; offset} in
  ({state with symbol_table= new_entry :: state.symbol_table}, offset)

(* Find a local variable in the symbol table. *)
let find_local_var state var_name =
  let rec find symbol_table var_name =
    match symbol_table with
    | [] -> raise Symbol_Not_Found
    | h :: t -> (
      match h with
      | Variable x -> if var_name = x.name then x.offset else find t var_name
      | _ -> find t var_name )
  in
  find state.symbol_table var_name

(* Emit a new opcode and return the resulting state. *)
let emit_opcode state opcode = {state with ops= opcode :: state.ops}

(* Return the identifier from a declarator. *)
let rec identifier (declarator : declarator) =
  let rec match_direct_declarator (direct_declarator : direct_declarator) =
    match direct_declarator with
    | Identifier x -> x
    | Declarator x -> identifier x
    | FunctionDeclarator x -> match_direct_declarator x.direct_declarator
  in
  match_direct_declarator declarator.direct_declarator

(* Store the value at the top of stack relative to the stack pointer. *)
let store_stack_rel state offset =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push (offset - 1))
    ; Arithmetic Addtion
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Load a value relative to the stack pointer. *)
let load_stack_rel state offset =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push (offset - 1))
    ; Arithmetic Addtion
    ; HeapAccess Retrieve ]
  in
  List.fold_left emit_opcode state ops

(* Load the value pointed to by the stack pointer and store it in the stack
   pointer. *)
let restore_stack_ptr state =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; HeapAccess Retrieve
    ; StackManipulation (Push 0)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Store a copy of stack pointer relative to the stack pointer. *)
let store_stack_ptr state offset =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push (offset - 1))
    ; Arithmetic Addtion
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Emit the built-in function puti and return the function label. *)
let emit_puti state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_fn state "puti" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load and output the argument. *)
  let state = load_stack_rel state (-1) in
  let state = emit_opcode state (IO OutputCharacter) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit opcodes for a primary expression. *)
let emit_primary_expression state x =
  match x with
  | Identifier x' ->
      let offset = find_local_var state x' in
      Printf.printf
        "emit_primary_expression: Identifier. Found %s at offset %d\n" x' offset ;
      load_stack_rel state offset
  | Constant x' ->
      Printf.printf "Emit Constant %d\n" x' ;
      emit_opcode state (StackManipulation (Push x'))
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

(* Emit a postfix expression. *)
let rec emit_postfix_expression state x =
  match x with
  | PrimaryExpression x' -> emit_primary_expression state x'
  | FunctionCall x' ->
      Printf.printf "emit_postfix_expression: FunctionCall\n" ;
      (* Store the function arguments relative to the stack pointer. *)
      let rec process_arguments state arguments =
        match arguments with
        | [] -> state
        | h :: t ->
            (* Compute the expression the place the result on top of the stack.
               Emit a temporary local variable. *)
            let state = emit_assignment_expression state h in
            let tmp_var_name = string_of_int (Random.int 65536) in
            let state, offset = add_local_var state "empty" tmp_var_name in
            Printf.printf "tmp_var %s offset %d\n" tmp_var_name offset ;
            let state = store_stack_rel state offset in
            process_arguments state t
      in
      (* Assume our postfix expression is an identifier. *)
      let function_name =
        match x'.postfix_expression with
        | PrimaryExpression x'' -> (
          match x'' with
          | Identifier x''' -> x'''
          | _ ->
              Printf.printf "No function name!\n" ;
              "" )
        | _ ->
            Printf.printf "No function name!\n" ;
            ""
      in
      let state = process_arguments state x'.argument_expression_list in
      (* Generate a new tmp var to store the current stack pointer. We are
         basically just bumping the local variable count. *)
      (*let stack_tmp_var_name = string_of_int (Random.int 65536) in let state,
        offset = add_local_var state "empty" stack_tmp_var_name in Printf.printf
        "stack var %s offset %d\n" stack_tmp_var_name offset ;*)
      (*let state = store_stack_ptr state offset in*)
      (* Find the function label and call the function. *)
      let label = find_fn_label state function_name in
      emit_opcode state (FlowControl (Call label))
      (*restore_stack_ptr state*)
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_unary_expression state x =
  match x with
  | PostfixExpression x' -> emit_postfix_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_multiplicative_expression state x =
  match x with
  | CastExpression x' -> emit_unary_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_additive_expression state x =
  match x with
  | MultiplicativeExpression x' -> emit_multiplicative_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_shift_expression state x =
  match x with
  | AdditiveExpression x' -> emit_additive_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_relational_expression state x =
  match x with
  | ShiftExpression x' -> emit_shift_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_equality_expression state x =
  match x with
  | RelationalExpression x' -> emit_relational_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_and_expression state x =
  match x with
  | EqualityExpression x' -> emit_equality_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_exclusive_or_expression state x =
  match x with
  | AndExpression x' -> emit_and_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_inclusive_or_expression state x =
  match x with
  | ExclusiveOr x' -> emit_exclusive_or_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_logical_and_expression state x =
  match x with
  | InclusiveOrExpression x' -> emit_inclusive_or_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_logical_or_expression state x =
  match x with
  | LogicalOrLogicalAndExpression x' -> emit_logical_and_expression state x'
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_conditional_expression state x =
  match x with
  | ContitionalLogicalOrExpression x' -> emit_logical_or_expression state x'
  | _ ->
      Printf.printf "emit_conditional_expression: Not implemented\n" ;
      state

and emit_assignment_expression state x =
  match x with
  | AssignmentConditionalExpression x' -> emit_conditional_expression state x'
  | _ ->
      Printf.printf "emit_assignment_expression: Not implemented\n" ;
      state

and emit_expression state (expression : expression) =
  match expression with
  | AssignmentExpression x ->
      Printf.printf "assignment expression\n" ;
      emit_assignment_expression state x

let rec emit_block_item state (block_item : block_item) =
  match block_item with
  | Declaration x ->
      Printf.printf "block_item: emit_declaration\n" ;
      emit_declaration state x
  | Statement x ->
      Printf.printf "block_item: emit_statement\n" ;
      emit_statement state x

and emit_statement state (statement : statement) =
  match statement with
  | LabeledStatement _ ->
      Printf.printf "LabeledStatement\n" ;
      state
  | CompoundStatement _ ->
      Printf.printf "CompoundStatement\n" ;
      state
  | ExpressionStatement x' -> (
      Printf.printf "ExpressionStatement\n" ;
      match x' with Some x'' -> emit_expression state x'' | None -> state )
  | SelectionStatement _ ->
      Printf.printf "SelectionStatement\n" ;
      state
  | IterationStatement _ ->
      Printf.printf "IterationStatement\n" ;
      state
  | JumpStatement _ ->
      Printf.printf "JumpStatement\n" ;
      state

(* Emit a function definition. *)
and emit_function_definition state (function_definition : function_definition) =
  let identifier = identifier function_definition.declarator in
  let state, label = add_fn state identifier in
  Printf.printf "Found function %s with label %s\n" identifier label ;
  let state = emit_opcode state (FlowControl (Mark label)) in
  let state =
    List.fold_left emit_block_item state function_definition.compound_statement
  in
  emit_opcode state (FlowControl EndSubroutine)

(* Emit a declaration, returning the new state of the generator. *)
and emit_declaration state declaration =
  (* Emit a declarator, returning the new state of the generator. *)
  let emit_init_declarator state (init_declarator : init_declarator) =
    (* Emit the (optional) initializer. *)
    let state, has_init =
      match init_declarator._initializer with
      | Some x ->
          let state = emit_assignment_expression state x in
          (state, true)
      | None -> (state, false)
    in
    (* Look up or add the name to the symbol table. *)
    let name = identifier init_declarator.declarator in
    let state, offset = add_local_var state "empty" name in
    Printf.printf "Name %s at offset %d\n" name offset ;
    (* Store the initial value if we had one. *)
    Printf.printf "delcaration has_init? %b\n" has_init ;
    if has_init then (
      Printf.printf "emitting stack manipulation ops \n" ;
      let state = emit_opcode state (StackManipulation (Push offset)) in
      let state = emit_opcode state (StackManipulation Swap) in
      let state = emit_opcode state (HeapAccess Store) in
      state )
    else state
  in
  match declaration.init_declarator_list with
  | Some x -> List.fold_left emit_init_declarator state x
  | None ->
      Printf.printf "No declarators\n" ;
      state

let emit_external_declaration state ast =
  match ast with
  | FunctionDefinition x ->
      Printf.printf "emit_external_declaration: FunctionDefinition\n" ;
      emit_function_definition state x
  | Declaration x ->
      Printf.printf "emit_external_declaration: FunctionDefinition\n" ;
      emit_declaration state x

let emit_prog_prolog state =
  (* Call main at label 0 and then end the program. *)
  let ops =
    [ StackManipulation (Push 0)
    ; StackManipulation (Push 1)
    ; HeapAccess Store
    ; FlowControl (Call "0")
    ; FlowControl EndProgram ]
  in
  List.fold_left emit_opcode state ops

(* Emit bytecode for built in functions, whether we call them or not. *)
let emit_build_ins state = emit_puti state

(* Generate Whitespace bytecode from an abstract syntac tree. *)
let generate (x : external_declaration list) =
  let state = {ops= []; symbol_table= []} in
  let state = emit_prog_prolog state in
  let state, _ = add_fn state "main" in
  let state = emit_build_ins state in
  let state = List.fold_left emit_external_declaration state x in
  List.rev state.ops
