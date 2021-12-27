open Ast
open Whitespace

exception Spacebar_Exception

exception Duplicate_Function_Definition

exception Symbol_Not_Found

exception Function_Not_Found

(* An entry in the symbol table. *)
type symbol =
  | Global of {scope: int; name: string; offset: int}
  | Function of {scope: int; name: string; label: string}
  | Argument of {scope: int; fn_name: string; name: string; offset: int}
  | Variable of {scope: int; name: string; offset: int}

(* The current state of the code generator. Stores the list of generated opcodes
   and the symbol table. The symbol table is a list of symbols, this isn't the
   most efficient structure, but its easy. *)
type state =
  { ops: imp list
  ; next_fn_label: int
  ; symbol_table: symbol list
  ; iter_stmt_start_label: string option
  ; iter_stmt_end_label: string option }

(* Return the next avaliable label. *)
let next_fn_label state =
  let label = state.next_fn_label + 1 in
  ({state with next_fn_label= label}, label)

(* Look up a name in the global section of the symbol table (scope zero) and
   return the associated label. *)
let find_fn_label state fn_name =
  let rec do_find symbol_table fn_name =
    match symbol_table with
    | [] -> raise Function_Not_Found
    | h :: t -> (
      match h with
      | Function x -> if x.name = fn_name then x.label else do_find t fn_name
      | _ -> do_find t fn_name )
  in
  do_find state.symbol_table fn_name

(* Add a function to the global section (scope zero) of the symbol table and
   return the function label. *)
let add_fn state fn_name =
  (* Bit of a hack...*)
  if fn_name = "main" then
    let entry = Function {scope= 0; name= fn_name; label= "0"} in
    ({state with symbol_table= entry :: state.symbol_table}, "0")
  else
    (* Get the next avaliable function label. Functions are labelled
       incrementally starting from '1' with label '0' reserved for 'main'.*)
    let state, label =
      let label = string_of_int state.next_fn_label in
      let next_fn_label = state.next_fn_label + 1 in
      ({state with next_fn_label}, label)
    in
    let symbol_table =
      Function {scope= 0; name= fn_name; label} :: state.symbol_table
    in
    ({state with symbol_table}, label)

(* Add a function argument to the global section (scope zero) of the symbol
   table. *)
let add_fn_arg state fn_name arg_name =
  (* Count the number of arguments the named function has. *)
  let rec count_args symbol_table fn_name i =
    match symbol_table with
    | [] -> i
    | h :: t -> (
      match h with
      | Argument x when x.fn_name = fn_name -> count_args t fn_name (i + 1)
      | _ -> count_args t fn_name i )
  in
  let offset = -2 - count_args state.symbol_table fn_name 0 in
  Printf.eprintf "Adding argument %s at offset %d to function %s\n" arg_name
    offset fn_name ;
  let new_entry = Argument {scope= 0; fn_name; name= arg_name; offset} in
  ({state with symbol_table= new_entry :: state.symbol_table}, offset)

(* Add a local variable to the symbol table and return its offset. *)
let add_local_var state fn_name var_name =
  (* Count the number of local variables this scope has. *)
  let rec count symbol_table fn_name i =
    match symbol_table with
    | [] -> i
    | h :: t -> (
      match h with
      | Variable x when x.scope = 0 -> count t fn_name (i + 1)
      | _ -> count t fn_name i )
  in
  let offset = count state.symbol_table fn_name 0 in
  let new_entry = Variable {scope= 0; name= var_name; offset} in
  ({state with symbol_table= new_entry :: state.symbol_table}, offset)

(* Find the offset of a symbol in the symbol table. Search starts at the current
   scope and works towards the global scope. *)
let find_offset state var_name =
  Printf.eprintf "find_offset %s\n" var_name ;
  let rec find symbol_table var_name =
    match symbol_table with
    | [] ->
        Printf.eprintf " Failed to find var %s\n" var_name ;
        raise Symbol_Not_Found
    | h :: t -> (
      match h with
      | Argument x -> if var_name = x.name then x.offset else find t var_name
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

(* Store a value on the stack. *)
let store_stack state =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation Swap
    ; HeapAccess Store
    ; StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push 1)
    ; Arithmetic Addtion
    ; StackManipulation (Push 0)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Take a stack with top [offset, value] and store value in (rbp + offset). *)
let store_stack_offset state =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
    ; Arithmetic Addtion
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Load a value relative to rbp. *)
let load_rbp_rel state offset =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
    ; StackManipulation (Push offset)
    ; Arithmetic Addtion
    ; HeapAccess Retrieve ]
  in
  List.fold_left emit_opcode state ops

(* Push a value offset relative to rbp onto the stack. *)
let push_rbp_rel_offset state offset =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
    ; StackManipulation (Push offset)
    ; Arithmetic Addtion ]
  in
  List.fold_left emit_opcode state ops

(* Store the current value of rsp in rbp. *)
let set_rsp state =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push 1)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Push the value of rbp onto the stack. *)
let push_rbp state =
  (* Load the value of rbp. *)
  let state = emit_opcode state (StackManipulation (Push 1)) in
  let state = emit_opcode state (HeapAccess Retrieve) in
  (* Store the value of rsp on the stack. *)
  store_stack state

(* Load the value of rbp from the stack. This is executed as part of a function
   epilogue, where the value of rbp is expected at (rsp - 1). *)
let pop_rbp state =
  (* Load the value of rsp. *)
  let state = load_rbp_rel state (-1) in
  let state = emit_opcode state (StackManipulation (Push 1)) in
  let state = emit_opcode state (StackManipulation Swap) in
  emit_opcode state (HeapAccess Store)

(* Adjust the stack pointer by `value`. *)
let add_rsp state value =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push value)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Store the current value of rsp in rbp. *)
let store_rsp state =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push 1)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Subtract a value from the stack pointer. *)
let sub_rsp state value =
  let ops =
    [ StackManipulation (Push 0)
    ; HeapAccess Retrieve
    ; StackManipulation (Push value)
    ; Arithmetic Subtraction
    ; StackManipulation (Push 0)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Store the current value of rbp in rsp. *)
let restore_rsp state =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
    ; StackManipulation (Push 0)
    ; StackManipulation Swap
    ; HeapAccess Store ]
  in
  List.fold_left emit_opcode state ops

(* Push the absolute address of [rbp + offset]. This is functionally equivilent
   to the x86 LEA instruction with the register fixed to rbp*)
let push_abs_addr state offset =
  (* Load rbp to the top of the stack. *)
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve (* Load rbp *)
    ; StackManipulation (Push offset)
    ; Arithmetic Addtion ]
    (* Add offset to get abs address. *)
  in
  List.fold_left emit_opcode state ops

(* Emit the built-in function geti and return the function label. *)
let emit_geti state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_fn state "geti" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load the output location and read the number. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO ReadNumber) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit the built-in function puti and return the function label. *)
let emit_puti state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_fn state "puti" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load and output the argument. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO OutputNumber) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit the built-in function putc and return the function label. *)
let emit_putc state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_fn state "putc" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load and output the argument. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO OutputCharacter) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit opcodes for a primary expression. *)
let rec emit_primary_expression state expr lvalue =
  match expr with
  | Identifier x' ->
      let offset = find_offset state x' in
      Printf.eprintf
        "emit_primary_expression: Identifier. Found %s at offset %d\n" x' offset ;
      if lvalue then emit_opcode state (StackManipulation (Push offset))
      else load_rbp_rel state offset
  | Constant x' ->
      Printf.eprintf "Emit Constant %d\n" x' ;
      emit_opcode state (StackManipulation (Push x'))
  | Expression x' -> emit_expression state x'
  | _ ->
      Printf.eprintf "emit_primary_expression: Not implemented\n" ;
      state

(* Emit a postfix expression. *)
and emit_postfix_expression state x lvalue =
  match x with
  | PrimaryExpression x' -> emit_primary_expression state x' lvalue
  | FunctionCall x' ->
      Printf.eprintf "emit_postfix_expression: FunctionCall\n" ;
      (* Store the function arguments relative to the stack pointer. *)
      let rec process_arguments state arguments =
        match arguments with
        | [] -> state
        | h :: t ->
            (* Compute the expression the place the result on top of the stack. *)
            let state = emit_assignment_expression state h in
            let state = store_stack state in
            process_arguments state t
      in
      (* Assume our postfix expression is an identifier. *)
      let function_name =
        match x'.postfix_expression with
        | PrimaryExpression x'' -> (
          match x'' with
          | Identifier x''' -> x'''
          | _ ->
              Printf.eprintf "No function name!\n" ;
              "" )
        | _ ->
            Printf.eprintf "No function name!\n" ;
            ""
      in
      (* Push the function arguments on the stack. *)
      let state =
        process_arguments state (List.rev x'.argument_expression_list)
      in
      (* Store the current frame pointer on the stack. *)
      let state = push_rbp state in
      (* Set the frame pointer for the callee. *)
      let state = set_rsp state in
      (* Find the function label and call the function. *)
      let label = find_fn_label state function_name in
      let state = emit_opcode state (FlowControl (Call label)) in
      (* Restore the frame and stack pointers.rbp and rsp *)
      let state = restore_rsp state in
      let state = pop_rbp state in
      sub_rsp state 1
  | _ ->
      Printf.eprintf "emit_postfix_expression: Not implemented\n" ;
      state

(* Lookup the offset of a unary expression in the symbol table. *)
and unary_expr_offset state expr =
  match expr with
  | PostfixExpression x -> (
    match x with
    | PrimaryExpression x' -> (
      match x' with
      | Identifier x'' -> find_offset state x''
      | _ -> raise Spacebar_Exception )
    | _ -> raise Spacebar_Exception )
  | _ -> raise Spacebar_Exception

and emit_unary_expression state x lvalue =
  match x with
  | PostfixExpression x' -> emit_postfix_expression state x' lvalue
  | PrefixIncrement _ ->
      Printf.eprintf "emit_unary_expression: PrefixIncrement Not implemented\n" ;
      state
  | PrefixDecrement _ ->
      Printf.eprintf "emit_unary_expression: PrefixDecrement Not implemented\n" ;
      state
  | UnaryOperator x' -> (
    match x'.operator with
    | AddressOf _ ->
        (* Look up the offset of the lvalue expression from rbp. *)
        let offset = unary_expr_offset state x'.unary_expression in
        Printf.eprintf "AddressOf: Pushed value %d from rbp to stack\n" offset ;
        (* Emit the absolute address of the expression. *)
        push_abs_addr state offset
    | PointerDereference _ ->
        Printf.eprintf "!!! pointer-dereference: %b\n" lvalue ;
        (* Evaluate the expression so the address is on top of the stack. *)
        let state = emit_unary_expression state x'.unary_expression lvalue in
        (* Load the value at the absolute address. *)
        emit_opcode state (HeapAccess Retrieve)
    | UnaryPlus _ ->
        Printf.eprintf "UnaryPlus Not implemented\n" ;
        state
    | UnaryMinus _ ->
        let state = emit_opcode state (StackManipulation (Push 0)) in
        let state = emit_unary_expression state x'.unary_expression lvalue in
        emit_opcode state (Arithmetic Subtraction)
    | UnaryBitwiseNot _ ->
        Printf.eprintf "UnaryBitwiseNot Not implemented\n" ;
        state
    | UnaryNot _ ->
        Printf.eprintf "UnaryNot Not implemented\n" ;
        state )

and emit_multiplicative_expression state x =
  match x with
  | CastExpression x' -> emit_unary_expression state x' false
  | MultiplicativeProduct x' ->
      let state =
        emit_multiplicative_expression state x'.multiplicative_expression
      in
      let state = emit_unary_expression state x'.cast_expression false in
      emit_opcode state (Arithmetic Multiplication)
  | MultiplicativeDivision x' ->
      let state =
        emit_multiplicative_expression state x'.multiplicative_expression
      in
      let state = emit_unary_expression state x'.cast_expression false in
      emit_opcode state (Arithmetic Division)
  | MultiplicativeRemainder x' ->
      let state =
        emit_multiplicative_expression state x'.multiplicative_expression
      in
      let state = emit_unary_expression state x'.cast_expression false in
      emit_opcode state (Arithmetic Modulo)

and emit_additive_expression state x =
  match x with
  | MultiplicativeExpression x' -> emit_multiplicative_expression state x'
  | AdditiveAdditionExpression x' ->
      let state = emit_additive_expression state x'.additive_expression in
      let state =
        emit_multiplicative_expression state x'.multiplicative_expression
      in
      emit_opcode state (Arithmetic Addtion)
  | AdditiveSubtractionExpression x' ->
      let state = emit_additive_expression state x'.additive_expression in
      let state =
        emit_multiplicative_expression state x'.multiplicative_expression
      in
      emit_opcode state (Arithmetic Subtraction)

and emit_shift_expression state x =
  match x with
  | AdditiveExpression x' -> emit_additive_expression state x'
  | _ ->
      Printf.eprintf "emit_shift_expression: Not implemented\n" ;
      state

and emit_relational_expression state x =
  match x with
  | ShiftExpression x' -> emit_shift_expression state x'
  | LessThanExpression x' ->
      (* True if (lhs - rhs) is negative. *)
      let state, negative_label = next_fn_label state in
      let state, end_label = next_fn_label state in
      let state = emit_relational_expression state x'.relational_expression in
      (* LHS *)
      let state = emit_shift_expression state x'.shift_expression in
      (* RHS *)
      let state = emit_opcode state (Arithmetic Subtraction) in
      (* LHS - RHS *)
      let state =
        emit_opcode state
          (FlowControl (JumpNegative (string_of_int negative_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      let state =
        emit_opcode state
          (FlowControl (UnconditionalJump (string_of_int end_label)))
      in
      let state =
        emit_opcode state (FlowControl (Mark (string_of_int negative_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      emit_opcode state (FlowControl (Mark (string_of_int end_label)))
  | GreaterThanExpression x' ->
      (* True if (lhs - rhs) is NOT negative. *)
      let state, negative_label = next_fn_label state in
      let state, end_label = next_fn_label state in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_shift_expression state x'.shift_expression in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state =
        emit_opcode state
          (FlowControl (JumpNegative (string_of_int negative_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state =
        emit_opcode state
          (FlowControl (UnconditionalJump (string_of_int end_label)))
      in
      let state =
        emit_opcode state (FlowControl (Mark (string_of_int negative_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      emit_opcode state (FlowControl (Mark (string_of_int end_label)))
  | LessThanEqualThanExpression x' ->
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_shift_expression state x'.shift_expression in
      state
  | GreaterThanEqualExpression x' ->
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_shift_expression state x'.shift_expression in
      state

and emit_equality_expression state x =
  match x with
  | RelationalExpression x' -> emit_relational_expression state x'
  | EqualToExpression x' ->
      let state, zero_label = next_fn_label state in
      let state, end_label = next_fn_label state in
      let state = emit_equality_expression state x'.equality_expression in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state =
        emit_opcode state (FlowControl (JumpZero (string_of_int zero_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      let state =
        emit_opcode state
          (FlowControl (UnconditionalJump (string_of_int end_label)))
      in
      let state =
        emit_opcode state (FlowControl (Mark (string_of_int zero_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      emit_opcode state (FlowControl (Mark (string_of_int end_label)))
  | NotEqualToExpression x' ->
      let state, zero_label = next_fn_label state in
      let state, end_label = next_fn_label state in
      let state = emit_equality_expression state x'.equality_expression in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state =
        emit_opcode state (FlowControl (JumpZero (string_of_int zero_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state =
        emit_opcode state
          (FlowControl (UnconditionalJump (string_of_int end_label)))
      in
      let state =
        emit_opcode state (FlowControl (Mark (string_of_int zero_label)))
      in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      emit_opcode state (FlowControl (Mark (string_of_int end_label)))

and emit_and_expression state x =
  match x with
  | EqualityExpression x' -> emit_equality_expression state x'
  | _ ->
      Printf.eprintf "emit_and_expression: Not implemented\n" ;
      state

and emit_exclusive_or_expression state x =
  match x with
  | AndExpression x' -> emit_and_expression state x'
  | _ ->
      Printf.eprintf "emit_exclusive_or_expression: Not implemented\n" ;
      state

and emit_inclusive_or_expression state x =
  match x with
  | ExclusiveOr x' -> emit_exclusive_or_expression state x'
  | _ ->
      Printf.eprintf "emit_inclusive_or_expression: Not implemented\n" ;
      state

and emit_logical_and_expression state x =
  match x with
  | InclusiveOrExpression x' -> emit_inclusive_or_expression state x'
  | _ ->
      Printf.eprintf "emit_logical_and_expression: Not implemented\n" ;
      state

and emit_logical_or_expression state x =
  match x with
  | LogicalOrLogicalAndExpression x' -> emit_logical_and_expression state x'
  | _ ->
      Printf.eprintf "emit_logical_or_expression: Not implemented\n" ;
      state

and emit_conditional_expression state x =
  match x with
  | ContitionalLogicalOrExpression x' -> emit_logical_or_expression state x'
  | _ ->
      Printf.eprintf "emit_conditional_expression: Not implemented\n" ;
      state

and emit_assignment_expression state x =
  match x with
  | AssignmentConditionalExpression x' ->
      Printf.eprintf "AssignmentConditionalExpression\n" ;
      emit_conditional_expression state x'
  | AssignmentOperation x ->
      Printf.eprintf "Emit assignment operation!!!!!!!\n" ;
      let state =
        match x.assignment_operator with
        | Assign _ ->
            (* Evalulate lhs expression to determine the l-value. Evaluation of
               this expression should leave the address of the value on top of
               the stack. At the moment, we can only assign to local variables. *)
            (* Emit rhs first. *)
            let state =
              emit_assignment_expression state x.assignment_expression
            in
            (* Make sure stack pointer relative offset is on top of the current
               stack*)
            let state = emit_unary_expression state x.unary_expression true in
            (* Store the value. *)
            store_stack_offset state
      in
      state

and emit_expression state (expression : expression) =
  match expression with
  | AssignmentExpression x ->
      Printf.eprintf "assignment expression\n" ;
      emit_assignment_expression state x

let rec emit_block_item state (block_item : block_item) =
  match block_item with
  | Declaration x ->
      Printf.eprintf "block_item: emit_declaration\n" ;
      emit_declaration state x
  | Statement x ->
      Printf.eprintf "block_item: emit_statement\n" ;
      emit_statement state x

and emit_statement state (statement : statement) =
  match statement with
  | LabeledStatement _ ->
      Printf.eprintf "LabeledStatement\n" ;
      state
  | CompoundStatement x ->
      Printf.eprintf "CompoundStatement\n" ;
      List.fold_left emit_block_item state x
  | ExpressionStatement x' -> (
      Printf.eprintf "ExpressionStatement\n" ;
      match x' with Some x'' -> emit_expression state x'' | None -> state )
  | SelectionStatement x -> (
      Printf.eprintf "SelectionStatement\n" ;
      match x with
      | If x' ->
          (* Label for conditional jump. *)
          let state, skip_condition_label = next_fn_label state in
          (* Evaluate expression and jump if false. *)
          let state = emit_expression state x'.expression in
          let state =
            emit_opcode state
              (FlowControl (JumpZero (string_of_int skip_condition_label)))
          in
          (* Emit body of condition. *)
          let state = emit_statement state x'.body in
          (* Mark label for skipping conditional body. *)
          emit_opcode state
            (FlowControl (Mark (string_of_int skip_condition_label)))
      | IfElse _ -> state
      | Switch _ -> state )
  | IterationStatement x -> (
      Printf.eprintf "IterationStatement\n" ;
      match x with
      | While x' ->
          (* Labels for condition and end. *)
          let state, condition_label = next_fn_label state in
          let state, end_label = next_fn_label state in
          let state =
            { state with
              iter_stmt_end_label= Some (string_of_int end_label)
            ; iter_stmt_start_label= Some (string_of_int condition_label) }
          in
          (* Mark start of loop, before expression. *)
          let state =
            emit_opcode state
              (FlowControl (Mark (string_of_int condition_label)))
          in
          (* Evaluate expression. *)
          let state = emit_expression state x'.expression in
          let state =
            emit_opcode state (FlowControl (JumpZero (string_of_int end_label)))
          in
          (* Emit body. *)
          let state = emit_statement state x'.body in
          (* Unconditional jump. *)
          let state =
            emit_opcode state
              (FlowControl (UnconditionalJump (string_of_int condition_label)))
          in
          (* End label *)
          emit_opcode state (FlowControl (Mark (string_of_int end_label)))
      | _ -> state )
  | JumpStatement x -> (
      Printf.eprintf "Emitting JumpStatement\n" ;
      match x with
      | Goto _ ->
          Printf.eprintf "Unsupported Goto Statement\n" ;
          state
      | Continue -> (
        (* Unconditionally jump to iteration statement condition label. *)
        match state.iter_stmt_start_label with
        | Some label -> emit_opcode state (FlowControl (UnconditionalJump label))
        | None -> raise Spacebar_Exception )
      | Break -> (
        (* Unconditionally jump to the currently active iteration statement end
           label. *)
        match state.iter_stmt_end_label with
        | Some label -> emit_opcode state (FlowControl (UnconditionalJump label))
        | None -> raise Spacebar_Exception )
      | Return x' -> (
        match x'.expression with
        | Some x'' ->
            let state = emit_expression state x'' in
            emit_opcode state (FlowControl EndSubroutine)
        | None -> emit_opcode state (FlowControl EndSubroutine) ) )

(* Emit a function definition. *)
and emit_fn_def state (fn_def : function_definition) =
  (* Add the function to the symbol table. *)
  let fn_name = identifier fn_def.declarator in
  let state, label = add_fn state fn_name in
  Printf.eprintf "Found function %s with label %s\n" fn_name label ;
  (* Add the function arguments to the symbol table. *)
  let state =
    match fn_def.declarator.direct_declarator with
    | FunctionDeclarator x ->
        (* Extract argument names. *)
        let args =
          List.map
            (fun (y : parameter_declaration) -> identifier y.declarator)
            x.parameter_list
        in
        (* Add arguments to symbol table. *)
        List.fold_left
          (fun acc x ->
            Printf.eprintf "Adding fn_arg %s for fn %s to symbol table\n" x
              fn_name ;
            let state, _ = add_fn_arg acc fn_name x in
            state )
          state args
    | _ ->
        Printf.eprintf "Unexpected declarator\n" ;
        state
  in
  (* Mark the function. *)
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Emit the function definition. *)
  let state = List.fold_left emit_block_item state fn_def.compound_statement in
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
    Printf.eprintf "Name %s at offset %d\n" name offset ;
    (* Store the initial value if we had one. *)
    Printf.eprintf "delcaration has_init? %b\n" has_init ;
    if has_init then (
      Printf.eprintf "emitting stack manipulation ops \n" ;
      store_stack state )
    else state
  in
  List.fold_left emit_init_declarator state declaration.init_declarator_list

let emit_external_declaration state ast =
  match ast with
  | FunctionDefinition x ->
      Printf.eprintf "emit_external_declaration: FunctionDefinition\n" ;
      emit_fn_def state x
  | Declaration x ->
      Printf.eprintf "emit_external_declaration: FunctionDefinition\n" ;
      emit_declaration state x

let emit_prog_prolog state =
  (* Set up rsp and rbp call main then end the program. *)
  let ops =
    [ StackManipulation (Push 0)
    ; StackManipulation (Push 2)
    ; HeapAccess Store
    ; StackManipulation (Push 1)
    ; StackManipulation (Push 2)
    ; HeapAccess Store
    ; FlowControl (Call "0")
    ; FlowControl EndProgram ]
  in
  List.fold_left emit_opcode state ops

(* Emit bytecode for built in functions, whether we call them or not. *)
let emit_build_ins state =
  let state = emit_puti state in
  let state = emit_putc state in
  emit_geti state

(* Generate Whitespace bytecode from an abstract syntac tree. *)
let generate (x : external_declaration list) =
  let state =
    { ops= []
    ; next_fn_label= 1
    ; symbol_table= []
    ; iter_stmt_end_label= None
    ; iter_stmt_start_label= None }
  in
  let state = emit_prog_prolog state in
  let state, _ = add_fn state "main" in
  let state = emit_build_ins state in
  let state = List.fold_left emit_external_declaration state x in
  List.rev state.ops
