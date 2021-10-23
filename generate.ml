open Ast
open Whitespace

exception Function_Not_Found of string

(* An entry in the symbol table. *)
type symbol = {name: string}

(* The current state of our code generator. *)
type state =
  {ops: imp list; symbol_table: symbol list; function_table: symbol list}

(* Add a new function to our symbol table. TODO: Deduplicated with add_name. *)
let add_function state name =
  let rec find_name symbol_table name i =
    match symbol_table with
    | [] -> None
    | h :: _ when h.name = name -> Some i
    | _ :: t -> find_name t name (i + 1)
  in
  match find_name state.symbol_table name 0 with
  | Some x -> (state, x)
  | None ->
      let symbol_table' = {name} :: state.symbol_table in
      ({state with symbol_table= symbol_table'}, List.length state.symbol_table)

(* Return the label associated with a function. *)
let find_function_label state name =
  let rec find_name function_table name i =
    match function_table with
    | [] -> None
    | h :: _ when h.name = name -> Some i
    | _ :: t -> find_name t name (i + 1)
  in
  find_name state.function_table name 0

(* Add a new name to the symbol table and returning its offset and the new
   generator state. Currently we assume all names live in the global scope. *)
let add_name state name =
  (* Find an existing name in our symbol table and return its offset. *)
  let rec find_name symbol_table name i =
    match symbol_table with
    | [] -> None
    | h :: _ when h.name = name -> Some i
    | _ :: t -> find_name t name (i + 1)
  in
  (* Do we already know about this name. If we do, return its position in the
     symbol table. If we don't, add it to the symbol table and return its
     position. *)
  match find_name state.symbol_table name 0 with
  | Some x -> (state, x)
  | None ->
      let symbol_table' = {name} :: state.symbol_table in
      ({state with symbol_table= symbol_table'}, List.length state.symbol_table)

let emit_opcode state opcode = {state with ops= opcode :: state.ops}

(* Return an identifier from a declarator. *)
let rec function_name (declarator : declarator) =
  let rec match_direct_declarator (direct_declarator : direct_declarator) =
    match direct_declarator with
    | Identifier x -> x
    | Declarator x -> function_name x
    | FunctionDeclarator x -> match_direct_declarator x.direct_declarator
  in
  match_direct_declarator declarator.direct_declarator

(* Emit the built-in function puti and return the function label. *)
let emit_puti state =
  Printf.printf "emit_puti\n" ;
  (* Sanity check. This method should only ever be called once. *)
  let _ =
    match find_function_label state "puti" with
    | Some _ ->
        raise (Invalid_argument "Built-in function puit(...) already defined")
    | None -> ()
  in
  (* Add the function to the symbol table. *)
  let state, label = add_function state "puti" in
  (* Add the function label. *)
  let state = emit_opcode state (FlowControl (Mark (string_of_int label))) in
  (* Load the stack pointer (heap_addr = 0). *)
  let state = emit_opcode state (StackManipulation (Push 0)) in
  let state = emit_opcode state (HeapAccess Retrieve) in
  (* Load the value pointed to by the stack pointer *)
  let state = emit_opcode state (HeapAccess Retrieve) in
  (* Output the argument. *)
  let state = emit_opcode state (IO OutputCharacter) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  (state, label)

let emit_primary_expression state x =
  Printf.printf "%d\n" (List.length state.symbol_table) ;
  match x with
  | Identifier x' ->
      Printf.printf "Emit Identifier %s\n" x' ;
      state
  | Constant x' ->
      Printf.printf "Emit Constant %d\n" x' ;
      emit_opcode state (StackManipulation (Push x'))
  | _ ->
      Printf.printf "emit_logical_or_expression: Not implemented\n" ;
      state

(* Load the stack pointer, incriment the value, store the stack pointer. *)
let inc_stack_ptr state =
  let state = emit_opcode state (StackManipulation (Push 0)) in
  let state = emit_opcode state (HeapAccess Retrieve) in
  let state = emit_opcode state (StackManipulation (Push 1)) in
  let state = emit_opcode state (Arithmetic Addtion) in
  let state = emit_opcode state (StackManipulation (Push 0)) in
  emit_opcode state (HeapAccess Store)

let rec emit_postfix_expression state x =
  match x with
  | PrimaryExpression x' -> emit_primary_expression state x'
  | FunctionCall x' ->
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
      (* First, store the current stack pointer. *)
      let state = emit_opcode state (StackManipulation (Push 0)) in
      let state = emit_opcode state (HeapAccess Retrieve) in
      let state = emit_opcode state (StackManipulation Duplicate) in
      let state = emit_opcode state (HeapAccess Store) in
      let state = inc_stack_ptr state in
      (* Store the function arguments relative to the stack pointer. *)
      let rec process_arguments state arguments =
        match arguments with
        | hd :: tl ->
            (* Compute the expression, the result will end up on top of the
               stack. *)
            let state = emit_assignment_expression state hd in
            (* Store the result offset from the stack pointer. *)
            let state = emit_opcode state (StackManipulation (Push 0)) in
            let state = emit_opcode state (HeapAccess Retrieve) in
            let state = emit_opcode state (StackManipulation Swap) in
            let state = emit_opcode state (HeapAccess Store) in
            let state = inc_stack_ptr state in
            process_arguments state tl
        | [] -> state
      in
      let state = process_arguments state x'.argument_expression_list in
      (* Find the function label and call the function. *)
      let state, label =
        match find_function_label state function_name with
        | Some x -> (state, x)
        | None -> (
          (* Before we declare this an error, check if we are trying to call a
             built-in we have not yet emitted. *)
          match function_name with
          | "puti" -> emit_puti state
          | _ ->
              raise
                (Function_Not_Found
                   ("Function " ^ function_name ^ " not found in symbol table.")
                ) )
      in
      let state =
        emit_opcode state (FlowControl (Call (string_of_int label)))
      in
      (* Restore the stack pointer *)
      state
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
  | AssignmentExpression x -> emit_assignment_expression state x

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

(* Emit a function definition *)
and emit_function_definition state (function_definition : function_definition) =
  let function_name = function_name function_definition.declarator in
  let state, label = add_function state function_name in
  Printf.printf "Found function %s with label %d\n" function_name label ;
  let state = emit_opcode state (FlowControl (Mark (string_of_int label))) in
  let state =
    match function_definition.compound_statement with
    | Some x' -> List.fold_left emit_block_item state x'
    | None ->
        Printf.printf "No compount_statement\n" ;
        state
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
          let state = emit_opcode state (StackManipulation (Push 0)) in
          (state, true)
      | None -> (state, false)
    in
    (* Look up or add the name to the symbol table. *)
    let name = function_name init_declarator.declarator in
    let state, offset = add_name state name in
    Printf.printf "Name %s at offset %d\n" name offset ;
    (* Store the initial value if we had one. *)
    Printf.printf "delcaration has_init? %b\n" has_init ;
    if has_init then (
      Printf.printf "emitting stack manipulation ops \n" ;
      let state = emit_opcode state (StackManipulation (Push offset)) in
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
  | FunctionDefinition x -> emit_function_definition state x
  | Declaration x -> emit_declaration state x

(* Initalize the stack pointer at heap-addr 0 to the value 1. *)
let init_stack_pointer state =
  let state = emit_opcode state (StackManipulation (Push 0)) in
  let state = emit_opcode state (StackManipulation (Push 1)) in
  emit_opcode state (HeapAccess Store)

(* Generate bytecode from our AST. *)
let generate (x : external_declaration list) =
  let state = {ops= []; symbol_table= []; function_table= []} in
  let state = init_stack_pointer state in
  (* Call main at reserved label 0 and end program. *)
  let state = emit_opcode state (FlowControl (Call "0")) in
  let state = emit_opcode state (FlowControl EndProgram) in
  (* Traverse AST and emit external-declarations. *)
  let state = List.fold_left emit_external_declaration state x in
  List.rev state.ops
