open Ast
open Whitespace
open Lib.Symbol

exception Spacebar_Exception

exception Duplicate_Function_Definition

exception Symbol_Not_Found

exception Function_Not_Found

(* An entry in the symbol table. *)
type symbol =
  | Global of {scope: int; name: string; offset: int}
  | Function of {scope: int; name: string; label: int}
  | Argument of {scope: int; fn_name: string; name: string; offset: int}
  | Variable of {scope: int; name: string; offset: int}

(* The current state of the code generator. Stores the list of generated opcodes
   and the symbol table. The symbol table is a list of symbols, this isn't the
   most efficient structure, but its easy. *)
type state =
  { ops: imp list
  ; symbol_table: symbol_table
  ; iter_stmt_start_label: int option
  ; iter_stmt_end_label: int option }

(* Wrap Symbol.add_label *)
let add_label_s state =
  let label, symbol_table = add_label state.symbol_table in
  ({state with symbol_table}, label)

(* Wrap Symbol.push_scope *)
let push_scope_s state =
  let symbol_table = push_scope state.symbol_table in
  {state with symbol_table}

(* Wrap Symbol.pop_scope *)
let pop_scope_s state =
  let symbol_table = pop_scope state.symbol_table in
  {state with symbol_table}

(* Wrap Symbol.add_func *)
let add_func_s state name =
  let label, symbol_table = add_func state.symbol_table name in
  ({state with symbol_table}, label)

(* Wrap Symbol.find_func *)
let find_func_s state name =
  let label = find_func state.symbol_table name in
  match label with Some l -> l | None -> raise Spacebar_Exception

(* Wrap Symbol.add_func_arg*)
let add_func_arg_s state func_name arg_name size =
  let offset, symbol_table =
    add_func_arg state.symbol_table func_name arg_name size
  in
  ({state with symbol_table}, offset)

(* Wrap Symbol.add_local_var*)
let add_local_var_s state name size =
  let offset, symbol_table = add_local_var state.symbol_table name size in
  ({state with symbol_table}, offset)

(* Wrap Symbol.find_offset*)
let find_offset_s state name =
  let offset = find_offset state.symbol_table name in
  offset

(* Emit a new opcode and return the resulting state. *)
let emit_opcode state opcode = {state with ops= opcode :: state.ops}

(* Try and reduce an assignment expression to a constant value. Used for e.g.
   compile time determination of the size of arrays. TODO: This is terrible,
   clean this up! *)
let constant_from_assignment_expr assignment_expr =
  match assignment_expr with
  | AssignmentConditionalExpression x -> (
    match x with
    | ContitionalLogicalOrExpression x -> (
      match x with
      | LogicalOrLogicalAndExpression x -> (
        match x with
        | InclusiveOrExpression x -> (
          match x with
          | ExclusiveOr x -> (
            match x with
            | AndExpression x -> (
              match x with
              | EqualityExpression x -> (
                match x with
                | RelationalExpression x -> (
                  match x with
                  | ShiftExpression x -> (
                    match x with
                    | AdditiveExpression x -> (
                      match x with
                      | MultiplicativeExpression x -> (
                        match x with
                        | CastExpression x -> (
                          match x with
                          | PostfixExpression x -> (
                            match x with
                            | PrimaryExpression x -> (
                              match x with
                              | Constant x -> x
                              | _ -> raise Spacebar_Exception )
                            | _ -> raise Spacebar_Exception )
                          | _ -> raise Spacebar_Exception )
                        | _ -> raise Spacebar_Exception )
                      | _ -> raise Spacebar_Exception )
                    | _ -> raise Spacebar_Exception )
                  | _ -> raise Spacebar_Exception )
                | _ -> raise Spacebar_Exception )
              | _ -> raise Spacebar_Exception )
            | _ -> raise Spacebar_Exception )
          | _ -> raise Spacebar_Exception )
        | _ -> raise Spacebar_Exception )
      | _ -> raise Spacebar_Exception )
    | _ -> raise Spacebar_Exception )
  | _ -> raise Spacebar_Exception

(* Return the identifier from a declarator. *)
let rec identifier (declarator : declarator) =
  let rec match_direct_declarator (direct_declarator : direct_declarator) =
    match direct_declarator with
    | Identifier x -> x
    | Declarator x -> identifier x
    | ArrayDeclarator x -> match_direct_declarator x.direct_declarator
    | FunctionDeclarator x -> match_direct_declarator x.direct_declarator
  in
  match_direct_declarator declarator.direct_declarator

(* Store a value on the stack relative to rsp. *)
let store_rsp_rel state =
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

(* Store the value on top of the stack at [rbp + offset] *)
let store_rbp_rel state offset =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
    ; StackManipulation (Push offset)
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

(* Load a value from memory using the offset at the top of the stack. *)
let load_rbp_rel_stack state =
  let ops =
    [ StackManipulation (Push 1)
    ; HeapAccess Retrieve
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
  let ops = [StackManipulation (Push 1); HeapAccess Retrieve] in
  let state = List.fold_left emit_opcode state ops in
  store_rsp_rel state

(* Load the value of rbp from the stack. This is executed as part of a function
   epilogue, where the value of rbp is expected at (rsp - 1). *)
let pop_rbp state =
  (* Load the value of rsp. *)
  let state = load_rbp_rel state (-1) in
  let ops =
    [StackManipulation (Push 1); StackManipulation Swap; HeapAccess Store]
  in
  List.fold_left emit_opcode state ops

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
  let state, label = add_func_s state "geti" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load the output location and read the number. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO ReadNumber) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit the built-in function puti and return the function label. *)
let emit_puti state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_func_s state "puti" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load and output the argument. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO OutputNumber) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Emit the built-in function putc and return the function label. *)
let emit_putc state =
  (* Add the function to the symbol table and emit the function label. *)
  let state, label = add_func_s state "putc" in
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Load and output the argument. *)
  let state = load_rbp_rel state (-2) in
  let state = emit_opcode state (IO OutputCharacter) in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  state

(* Return an identifier from a postfix expression. *)
let id_from_postfix postfix_expr =
  match postfix_expr with
  | PrimaryExpression x'' -> (
    match x'' with IdentifierExpr x''' -> x''' | _ -> raise Spacebar_Exception )
  | _ -> raise Spacebar_Exception

(* Emit opcodes for a primary expression. *)
let rec emit_primary_expression state expr lvalue =
  match expr with
  | IdentifierExpr x' ->
      let offset = find_offset_s state x' in
      if lvalue then emit_opcode state (StackManipulation (Push offset))
      else load_rbp_rel state offset
  | Constant x' -> emit_opcode state (StackManipulation (Push x'))
  | Expression x' -> emit_expression state x'
  | _ -> state

(* Emit a postfix expression. *)
and emit_postfix_expression state x lvalue =
  match x with
  | PrimaryExpression x' -> emit_primary_expression state x' lvalue
  | ArrayAccess x' ->
      (* Emit the index expression. *)
      let state = emit_expression state x'.expression in
      (* Get the offset of the array start. *)
      let offset =
        unary_expr_offset state (PostfixExpression x'.postfix_expression)
      in
      (* Add the index to the offset. *)
      let state = emit_opcode state (StackManipulation (Push offset)) in
      let state = emit_opcode state (Arithmetic Addtion) in
      if lvalue then state else load_rbp_rel_stack state
  | FunctionCall x' ->
      (* Store the function arguments relative to the stack pointer. *)
      let rec process_arguments state arguments =
        match arguments with
        | [] -> state
        | h :: t ->
            (* Compute the expression the place the result on top of the stack. *)
            let state = emit_assignment_expression state h in
            let state = store_rsp_rel state in
            process_arguments state t
      in
      (* Assume our postfix expression is an identifier. *)
      let function_name = id_from_postfix x'.postfix_expression in
      (* Push the function arguments on the stack. *)
      let state =
        process_arguments state (List.rev x'.argument_expression_list)
      in
      (* Store the current frame pointer on the stack. *)
      let state = push_rbp state in
      (* Set the frame pointer for the callee. *)
      let state = set_rsp state in
      (* Find the function label and call the function. *)
      let label = find_func_s state function_name in
      let state = emit_opcode state (FlowControl (Call label)) in
      (* Restore the frame and stack pointers.rbp and rsp *)
      let state = restore_rsp state in
      let state = pop_rbp state in
      sub_rsp state 1
  | PostfixIncrement x' ->
      let offset =
        unary_expr_offset state (PostfixExpression x'.postfix_expression)
      in
      let state = load_rbp_rel state offset in
      let state = emit_opcode state (StackManipulation Duplicate) in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state = emit_opcode state (Arithmetic Addtion) in
      let state = emit_opcode state (StackManipulation (Push offset)) in
      store_stack_offset state
  | PostfixDecrement x' ->
      let offset =
        unary_expr_offset state (PostfixExpression x'.postfix_expression)
      in
      let state = load_rbp_rel state offset in
      let state = emit_opcode state (StackManipulation Duplicate) in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state = emit_opcode state (StackManipulation (Push offset)) in
      store_stack_offset state
  | _ -> state

(* Lookup the offset of a unary expression in the symbol table. *)
and unary_expr_offset state expr =
  match expr with
  | PostfixExpression x -> (
    match x with
    | PrimaryExpression x' -> (
      match x' with
      | IdentifierExpr x'' -> find_offset_s state x''
      | _ -> raise Spacebar_Exception )
    | ArrayAccess {postfix_expression= expr; _} ->
        unary_expr_offset state (PostfixExpression expr)
    | _ -> raise Spacebar_Exception )
  | _ -> raise Spacebar_Exception

and emit_unary_expression state x lvalue =
  match x with
  | PostfixExpression x' -> emit_postfix_expression state x' lvalue
  | PrefixIncrement x' ->
      let offset = unary_expr_offset state x'.unary_expression in
      let state = load_rbp_rel state offset in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state = emit_opcode state (Arithmetic Addtion) in
      let state = emit_opcode state (StackManipulation Duplicate) in
      let state = emit_opcode state (StackManipulation (Push offset)) in
      store_stack_offset state
  | PrefixDecrement x' ->
      let offset = unary_expr_offset state x'.unary_expression in
      let state = load_rbp_rel state offset in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state = emit_opcode state (StackManipulation Duplicate) in
      let state = emit_opcode state (StackManipulation (Push offset)) in
      store_stack_offset state
  | UnaryOperator x' -> (
    match x'.operator with
    | AddressOf _ ->
        (* Look up the offset of the lvalue expression from rbp. *)
        let offset = unary_expr_offset state x'.unary_expression in
        (* Emit the absolute address of the expression. *)
        push_abs_addr state offset
    | PointerDereference _ ->
        (* Evaluate the expression so the address is on top of the stack. *)
        let state = emit_unary_expression state x'.unary_expression lvalue in
        (* Load the value at the absolute address. *)
        emit_opcode state (HeapAccess Retrieve)
    | UnaryPlus _ ->
        (* noop *)
        state
    | UnaryMinus _ ->
        let state = emit_opcode state (StackManipulation (Push 0)) in
        let state = emit_unary_expression state x'.unary_expression lvalue in
        emit_opcode state (Arithmetic Subtraction)
    | UnaryBitwiseNot _ -> state
    | UnaryNot _ -> state )

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
  | _ -> state

and emit_relational_expression state x =
  match x with
  | ShiftExpression x' -> emit_shift_expression state x'
  | LessThanExpression x' ->
      (* True if (lhs - rhs) is negative. *)
      let state, negative_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_shift_expression state x'.shift_expression in
      let ops =
        [ Arithmetic Subtraction
        ; FlowControl (JumpNegative negative_label)
        ; StackManipulation (Push 0)
        ; FlowControl (UnconditionalJump end_label)
        ; FlowControl (Mark negative_label)
        ; StackManipulation (Push 1)
        ; FlowControl (Mark end_label) ]
      in
      List.fold_left emit_opcode state ops
  | GreaterThanExpression x' ->
      (* True if (lhs - rhs) is NOT negative. *)
      let state, negative_label = add_label_s state in
      let state, zero_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_shift_expression state x'.shift_expression in
      let ops =
        [ Arithmetic Subtraction
        ; StackManipulation Duplicate
        ; FlowControl (JumpNegative negative_label)
        ; FlowControl (JumpZero zero_label)
        ; StackManipulation (Push 1)
        ; FlowControl (UnconditionalJump end_label)
        ; FlowControl (Mark negative_label)
        ; StackManipulation Discard
        ; FlowControl (Mark zero_label)
        ; StackManipulation (Push 0)
        ; FlowControl (Mark end_label) ]
      in
      List.fold_left emit_opcode state ops
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
      let state, zero_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_equality_expression state x'.equality_expression in
      let state = emit_relational_expression state x'.relational_expression in
      let ops =
        [ Arithmetic Subtraction
        ; FlowControl (JumpZero zero_label)
        ; StackManipulation (Push 0)
        ; FlowControl (UnconditionalJump end_label)
        ; FlowControl (Mark zero_label)
        ; StackManipulation (Push 1)
        ; FlowControl (Mark end_label) ]
      in
      List.fold_left emit_opcode state ops
  | NotEqualToExpression x' ->
      let state, zero_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_equality_expression state x'.equality_expression in
      let state = emit_relational_expression state x'.relational_expression in
      let state = emit_opcode state (Arithmetic Subtraction) in
      let state = emit_opcode state (FlowControl (JumpZero zero_label)) in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state =
        emit_opcode state (FlowControl (UnconditionalJump end_label))
      in
      let state = emit_opcode state (FlowControl (Mark zero_label)) in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      emit_opcode state (FlowControl (Mark end_label))

and emit_and_expression state x =
  match x with
  | EqualityExpression x' -> emit_equality_expression state x'
  | _ -> state

and emit_exclusive_or_expression state x =
  match x with AndExpression x' -> emit_and_expression state x' | _ -> state

and emit_inclusive_or_expression state x =
  match x with
  | ExclusiveOr x' -> emit_exclusive_or_expression state x'
  | _ -> state

and emit_logical_and_expression state x =
  match x with
  | InclusiveOrExpression x' -> emit_inclusive_or_expression state x'
  | LogicalAndExpression x' ->
      let state, zero_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_logical_and_expression state x'.logical_and_expression in
      let state = emit_opcode state (FlowControl (JumpZero zero_label)) in
      let state =
        emit_inclusive_or_expression state x'.inclusive_or_expression
      in
      let state = emit_opcode state (FlowControl (JumpZero zero_label)) in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state =
        emit_opcode state (FlowControl (UnconditionalJump end_label))
      in
      let state = emit_opcode state (FlowControl (Mark zero_label)) in
      let state = emit_opcode state (StackManipulation (Push 0)) in
      emit_opcode state (FlowControl (Mark end_label))

and emit_logical_or_expression state x =
  match x with
  | LogicalOrLogicalAndExpression x' -> emit_logical_and_expression state x'
  | LogicalOrExpression x' ->
      let state, rhs_label = add_label_s state in
      let state, end_label = add_label_s state in
      let state = emit_logical_or_expression state x'.logical_or_expression in
      let state = emit_opcode state (FlowControl (JumpZero rhs_label)) in
      let state = emit_opcode state (StackManipulation (Push 1)) in
      let state =
        emit_opcode state (FlowControl (UnconditionalJump end_label))
      in
      let state = emit_opcode state (FlowControl (Mark rhs_label)) in
      let state = emit_logical_and_expression state x'.logical_and_expression in
      emit_opcode state (FlowControl (Mark end_label))

and emit_conditional_expression state x =
  match x with
  | ContitionalLogicalOrExpression x' -> emit_logical_or_expression state x'
  | _ -> state

and emit_assignment_expression state x =
  match x with
  | AssignmentConditionalExpression x' -> emit_conditional_expression state x'
  | AssignmentOperation x ->
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
  | AssignmentExpression x -> emit_assignment_expression state x

let rec emit_block_item state (block_item : block_item) =
  match block_item with
  | Declaration x -> emit_declaration state x
  | Statement x -> emit_statement state x

and emit_statement state (statement : statement) =
  match statement with
  | LabeledStatement _ -> state
  | CompoundStatement x ->
      let state = push_scope_s state in
      let state = List.fold_left emit_block_item state x in
      pop_scope_s state
  | ExpressionStatement x' -> (
    match x' with Some x'' -> emit_expression state x'' | None -> state )
  | SelectionStatement x -> (
    match x with
    | If x' ->
        (* Label for conditional jump. *)
        let state, skip_condition_label = add_label_s state in
        (* Evaluate expression and jump if false. *)
        let state = emit_expression state x'.expression in
        let state =
          emit_opcode state (FlowControl (JumpZero skip_condition_label))
        in
        (* Emit body of condition. *)
        let state = emit_statement state x'.body in
        (* Mark label for skipping conditional body. *)
        emit_opcode state (FlowControl (Mark skip_condition_label))
    | IfElse _ -> state
    | Switch _ -> state )
  | IterationStatement x -> (
    match x with
    | While x' ->
        (* Labels for condition and end. *)
        let state, condition_label = add_label_s state in
        let state, end_label = add_label_s state in
        let state =
          { state with
            iter_stmt_end_label= Some end_label
          ; iter_stmt_start_label= Some condition_label }
        in
        (* Mark start of loop, before expression. *)
        let state = emit_opcode state (FlowControl (Mark condition_label)) in
        (* Evaluate expression. *)
        let state = emit_expression state x'.expression in
        let state = emit_opcode state (FlowControl (JumpZero end_label)) in
        (* Emit body. *)
        let state = emit_statement state x'.body in
        (* Unconditional jump. *)
        let state =
          emit_opcode state (FlowControl (UnconditionalJump condition_label))
        in
        (* End label *)
        emit_opcode state (FlowControl (Mark end_label))
    | DoWhile x' ->
        (* Labels for condition and end. *)
        let state, body_label = add_label_s state in
        let state, end_label = add_label_s state in
        let state =
          { state with
            iter_stmt_end_label= Some end_label
          ; iter_stmt_start_label= Some body_label }
        in
        (* Mark start of loop, before expression. *)
        let state = emit_opcode state (FlowControl (Mark body_label)) in
        (* Emit body. *)
        let state = emit_statement state x'.body in
        (* Evaluate expression. *)
        let state = emit_expression state x'.expression in
        let state = emit_opcode state (FlowControl (JumpZero end_label)) in
        (* Unconditional jump. *)
        let state =
          emit_opcode state (FlowControl (UnconditionalJump body_label))
        in
        (* End label *)
        emit_opcode state (FlowControl (Mark end_label))
    | For x' ->
        (* Push a new block scope. *)
        let state = push_scope_s state in
        let state, loop_start_label = add_label_s state in
        let state, loop_end_label = add_label_s state in
        let state =
          { state with
            iter_stmt_end_label= Some loop_end_label
          ; iter_stmt_start_label= Some loop_start_label }
        in
        (* Emit loop variable declaration. *)
        let state =
          match x'.init_clause with
          | Some x'' -> (
            match x'' with
            | ForInitExpr x''' -> emit_expression state x'''
            | ForInitDecl x''' -> emit_declaration state x''' )
          | None -> state
        in
        (* Mark start of loop, before condition. *)
        let state = emit_opcode state (FlowControl (Mark loop_start_label)) in
        (* Evaluate condition. *)
        let state =
          match x'.condition with
          | Some x'' -> emit_expression state x''
          | None -> state
        in
        (* Jump if condition is not valid. *)
        let state = emit_opcode state (FlowControl (JumpZero loop_end_label)) in
        (* Emit body. *)
        let state = emit_statement state x'.body in
        (* Evaluate iteration statement. *)
        let state =
          match x'.iteration with
          | Some x'' -> emit_expression state x''
          | None -> state
        in
        (* Unconditional jump back to condition. *)
        let state =
          emit_opcode state (FlowControl (UnconditionalJump loop_start_label))
        in
        (* End label *)
        let state = emit_opcode state (FlowControl (Mark loop_end_label)) in
        (* Pop the loop block scope. *)
        pop_scope_s state )
  | JumpStatement x -> (
    match x with
    | Goto _ -> state
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

(* Every function gets a fixed size amount of stack space for locals. *)
and calc_func_stack_size = 8

and reserve_stack_space state size =
  let rec do_reserve state size =
    match size with
    | 0 -> state
    | n ->
        let state = emit_opcode state (StackManipulation (Push 0)) in
        let state = store_rsp_rel state in
        do_reserve state (n - 1)
  in
  do_reserve state size

(* Emit a function definition. *)
and emit_fn_def state (fn_def : function_definition) =
  (* Add the function to the symbol table. *)
  let fn_name = identifier fn_def.declarator in
  let state, label = add_func_s state fn_name in
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
            let state, _ = add_func_arg_s acc fn_name x 1 in
            state )
          state args
    | _ -> state
  in
  (* Mark the function. *)
  let state = emit_opcode state (FlowControl (Mark label)) in
  (* Push a new scope and emit the function definition. *)
  let state = push_scope_s state in
  (* Calculate stack space required for local variables. *)
  let state = reserve_stack_space state calc_func_stack_size in
  (* Allocate stack space for local variables. *)
  let state = List.fold_left emit_block_item state fn_def.compound_statement in
  let state = emit_opcode state (FlowControl EndSubroutine) in
  (* Pop the function scope. *)
  pop_scope_s state

(* Return true if a declaration is an array declaration. *)
and is_array_decl (decl : init_declarator) =
  match decl.declarator.direct_declarator with
  | ArrayDeclarator _ -> true
  | _ -> false

(* Emit a declaration, returning the new state of the generator. *)
and emit_declaration state declaration =
  (* Emit a non-array declarator, returning the new state of the generator. *)
  let emit_non_array_init_declarator state (init_declarator : init_declarator) =
    (* Emit the (optional) initializer. *)
    let state, has_init =
      match init_declarator._initializer with
      | Some x -> (emit_assignment_expression state x, true)
      | None -> (state, false)
    in
    (* Add the identifier to the symbol table. *)
    let id = identifier init_declarator.declarator in
    let state, offset = add_local_var_s state id 1 in
    (* Store the initial value if we had one. *)
    if has_init then store_rbp_rel state offset else state
  in
  (* Emit an array declarator, returning the new state of the generator. *)
  let emit_array_init_decl state (init_decl : init_declarator) =
    (* Most Whitespace interpreters use a hash table like structure for tracking
       stack memory and therefore don't do so well when reading memory locations
       that have not previously been written. To work around this we 'allocate'
       an array on the stack by zeroing out the amount of stack space we want. *)
    let rec stack_allocate state size =
      match size with
      | 0 -> state
      | n ->
          let state = emit_opcode state (StackManipulation (Push 0)) in
          let state = store_rsp_rel state in
          stack_allocate state (n - 1)
    in
    let name = identifier init_decl.declarator in
    (* Assume array sizes are constant primary expressions. *)
    let size =
      match init_decl.declarator.direct_declarator with
      | ArrayDeclarator x ->
          constant_from_assignment_expr x.assignment_expression
      | _ ->
          Printf.eprintf "Unsupported array size expression" ;
          raise Spacebar_Exception
    in
    let state, _ = add_local_var_s state name size in
    stack_allocate state size
  in
  let array_decl, non_array_decl =
    List.partition (fun x -> is_array_decl x) declaration.init_declarator_list
  in
  (* Handle normal declarations. *)
  let state =
    List.fold_left emit_non_array_init_declarator state non_array_decl
  in
  (* Handle array declarations. *)
  List.fold_left emit_array_init_decl state array_decl

let emit_external_declaration state ast =
  match ast with
  | FunctionDefinition x -> emit_fn_def state x
  | Declaration x -> emit_declaration state x

(* Emit program prolog. Set up rsp and rbp, call main then end the program. *)
let emit_prog_prolog state =
  let ops =
    [ StackManipulation (Push 0)
    ; StackManipulation (Push 2)
    ; HeapAccess Store
    ; StackManipulation (Push 1)
    ; StackManipulation (Push 2)
    ; HeapAccess Store
    ; FlowControl (Call 0)
    ; FlowControl EndProgram ]
  in
  List.fold_left emit_opcode state ops

(* Emit bytecode for built in functions, whether we call them or not. *)
let emit_build_ins state =
  let state = emit_puti state in
  let state = emit_putc state in
  emit_geti state

(* Generate Whitespace bytecode from an abstract syntax tree. *)
let generate (ast : external_declaration list) =
  let state =
    { ops= []
    ; symbol_table= new_symbol_table
    ; iter_stmt_end_label= None
    ; iter_stmt_start_label= None }
  in
  let state = emit_prog_prolog state in
  let state = emit_build_ins state in
  let state = List.fold_left emit_external_declaration state ast in
  List.rev state.ops
