exception Symbol_Not_Found

(* symbol represents an entry in the symbol table. *)
type symbol =
  | Function of {name: string; label: int}
  | FunctionArg of {func_name: string; name: string; size: int; offset: int}
  | Variable of {name: string; size: int; offset: int}

type symbol_table =
  { globals: symbol list
  ; scopes: symbol list Stack.t
  ; next_function_label: int
  ; scope: int }

(* Construct a new symbol table. Symbol tables are always constructed with the
   "main" function at label 0. *)
let new_symbol_table =
  let main_func = Function {name= "main"; label= 0} in
  { globals= [main_func]
  ; scopes= Stack.create ()
  ; next_function_label= 1
  ; scope= 0 }

(* Add a new label to the symbol table and return the label. *)
let add_label symbol_table =
  let label = symbol_table.next_function_label in
  let next_function_label = symbol_table.next_function_label + 1 in
  (label, {symbol_table with next_function_label})

(* Push a new scope. Typically called on the creation of a new block. *)
let push_scope symbol_table =
  let _ = Stack.push [] symbol_table.scopes in
  symbol_table

(* Discard the current scope. Typically called on the destruction of an existing
   block. *)
let pop_scope symbol_table =
  let _ = Stack.pop symbol_table.scopes in
  symbol_table

(* Add a new function to the global symbol table, returning the function label
   and the new symbol table. *)
let add_func symbol_table name =
  if name = "main" then (0, symbol_table)
  else
    let function_label = symbol_table.next_function_label in
    let globals' =
      Function {name; label= function_label} :: symbol_table.globals
    in
    let next_function_label = symbol_table.next_function_label + 1 in
    (function_label, {symbol_table with globals= globals'; next_function_label})

(* Find a function in the global symbol table. *)
let find_func symbol_table name =
  let do_find_func func_name symbol =
    match symbol with
    | Function {name; _} when name = func_name -> true
    | _ -> false
  in
  match List.find_opt (do_find_func name) symbol_table.globals with
  | Some f -> ( match f with Function f -> Some f.label | _ -> None )
  | None -> None

(* Add a function argument to the global symbol table. *)
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
  let offset = -2 - sum_args symbol_table.globals func_name 0 in
  let globals' =
    FunctionArg {func_name; name= arg_name; size; offset}
    :: symbol_table.globals
  in
  (offset, {symbol_table with globals= globals'})

(* Add a local variable to the current scope of the symbol table. *)
let add_local_var symbol_table name size =
  (* Helper method to sum the size of the other local variables in this scope. *)
  let rec sum_locals size symbols =
    match symbols with
    | [] -> size
    | h :: t -> (
      match h with
      | Variable {size= s; _} -> sum_locals (size + s) t
      | _ -> sum_locals size t )
  in
  (* Calculate the offset.*)
  let offset = Stack.fold sum_locals 0 symbol_table.scopes in
  (* Update the current scope. *)
  let scope = Stack.pop symbol_table.scopes in
  let scope = Variable {name; size; offset} :: scope in
  let _ = Stack.push scope symbol_table.scopes in
  (offset, symbol_table)

(* Find the offset of a variable symbol in the symbol table. *)
let find_offset symbol_table name =
  (* Helper function to accumulate all the offsets of a symbol within a scope. *)
  let rec find_matches name matches symbol_table =
    match symbol_table with
    | [] -> matches
    | h :: t -> (
      match h with
      | FunctionArg x ->
          if name = x.name then find_matches name (x.offset :: matches) t
          else find_matches name matches t
      | Variable x ->
          if name = x.name then find_matches name (x.offset :: matches) t
          else find_matches name matches t
      | _ -> find_matches name matches t )
  in
  (* Try and find the symbol in the stack of scopes. *)
  let matches =
    List.rev (Stack.fold (find_matches name) [] symbol_table.scopes)
  in
  if List.length matches > 0 then List.hd matches
  else
    (* If we didn't manage to find the symbol locally, check the global table. *)
    let global_matches = List.rev (find_matches name [] symbol_table.globals) in
    List.hd global_matches
