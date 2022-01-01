exception Symbol_Not_Found

(* symbol represents an entry in the symbol table. *)
type symbol =
  | Function of {name: string; label: int}
  | FunctionArg of {func_name: string; name: string; size: int; offset: int}
  | Variable of {func_name: string; name: string; size: int; offset: int}

type symbol_table = {symbols: symbol list; next_function_label: int; scope: int}

(* Construct a new symbol table. Symbol tables are always constructed with the
   "main" function at label 0. *)
let new_symbol_table =
  let main_func = Function {name= "main"; label= 0} in
  {symbols= [main_func]; next_function_label= 1; scope= 0}

(* Add a new function to the symbol table, returning the function label and the
   new symbol table. *)
let add_func symbol_table name =
  let function_label = symbol_table.next_function_label in
  let new_symbol_list =
    Function {name; label= function_label} :: symbol_table.symbols
  in
  let next_function_label = symbol_table.next_function_label + 1 in
  ( function_label
  , {symbol_table with symbols= new_symbol_list; next_function_label} )

(* Find a function in the symbol table. *)
let find_func symbol_table name =
  let do_find_func func_name symbol =
    match symbol with
    | Function {name; _} when name = func_name -> true
    | _ -> false
  in
  List.find_opt (do_find_func name) symbol_table.symbols

(* Add a function argument to the symbol table. *)
let add_func_arg symbol_table func_name arg_name size =
  (* Sum the size of the existing function arguments to get the offset. *)
  let rec sum_args symbols func_name size =
    match symbols with
    | [] -> size
    | h :: t -> (
      match h with
      | FunctionArg {size= s; func_name= n; _} when n = func_name ->
          sum_args t func_name (size + s)
      | _ -> sum_args t func_name size )
  in
  let offset = -2 - sum_args symbol_table.symbols func_name 0 in
  let new_symbol_list =
    FunctionArg {func_name; name= arg_name; size; offset}
    :: symbol_table.symbols
  in
  (offset, {symbol_table with symbols= new_symbol_list})

(* Add a local variable to the current scope of the symbol table. *)
let add_local_var symbol_table func_name name size =
  (* Sum the size of the other local variables in this scope to get the offset. *)
  let rec sum_locals symbols func_name size =
    match symbols with
    | [] -> size
    | h :: t -> (
      match h with
      | Variable {size= s; func_name= n; _} when n = func_name ->
          sum_locals t func_name (size + s)
      | _ -> sum_locals t func_name size )
  in
  let offset = sum_locals symbol_table.symbols func_name 0 in
  let new_symbol_list =
    Variable {func_name; name; size; offset} :: symbol_table.symbols
  in
  (offset, {symbol_table with symbols= new_symbol_list})

(* Find the offset of a variable symbol in the symbol table. *)
let find_offset symbol_table name =
  let rec do_find_offset symbol_table name =
    match symbol_table with
    | [] -> raise Symbol_Not_Found
    | h :: t -> (
      match h with
      | FunctionArg x ->
          if name = x.name then x.offset else do_find_offset t name
      | Variable x -> if name = x.name then x.offset else do_find_offset t name
      | _ -> do_find_offset t name )
  in
  do_find_offset symbol_table name
