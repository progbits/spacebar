open OUnit2
open Lib.Symbol

(* Adding 'main' function to symbol table should always just return label 0. *)
let test_add_main_function _ =
  let symbol_table = new_symbol_table in
  let label, _ = add_func symbol_table "main" in
  assert_equal 0 label

(* Symbol table should always contain an entry for the "main" function with
   label 0. *)
let test_find_main_function _ =
  let symbol_table = new_symbol_table in
  match find_func symbol_table "main" with
  | Some f -> assert_equal 0 f
  | None -> assert_failure "Function not found"

(* Add a new function to the symbol table. *)
let test_add_function _ =
  let symbol_table = new_symbol_table in
  let returned_label, _ = add_func symbol_table "foo" in
  assert_equal 1 returned_label

(* Retrieve a function label from the symbol table. *)
let test_find_function _ =
  let symbol_table = new_symbol_table in
  let _, symbol_table = add_func symbol_table "foo" in
  match find_func symbol_table "foo" with
  | Some f -> assert_equal 1 f
  | None -> assert_failure "Function not found"

(* Add multiple functions to the symbol table and ensure their labels are
   sequential. *)
let test_add_multiple_functions _ =
  let symbol_table = new_symbol_table in
  let _, symbol_table = add_func symbol_table "foo" in
  let _, symbol_table = add_func symbol_table "bar" in
  let _, symbol_table = add_func symbol_table "baz" in
  match find_func symbol_table "baz" with
  | Some f -> assert_equal 3 f
  | None -> assert_failure "Function not found"

(* Add function arguments to the symbol table. *)
let test_add_function_argument _ =
  let symbol_table = new_symbol_table in
  let _, symbol_table = add_func symbol_table "foo" in
  let a_offset, symbol_table = add_func_arg symbol_table "foo" "a" 1 in
  let b_offset, symbol_table = add_func_arg symbol_table "foo" "b" 13 in
  let c_offset, _ = add_func_arg symbol_table "foo" "c" 1 in
  assert_equal (-2) a_offset ;
  assert_equal (-3) b_offset ;
  assert_equal (-16) c_offset

(* Add local variables to the symbol table. *)
let test_add_local_variable _ =
  let symbol_table = new_symbol_table in
  let _, symbol_table = add_func symbol_table "foo" in
  let a_offset, symbol_table = add_local_var symbol_table "foo" "a" 8 in
  let b_offset, _ = add_local_var symbol_table "foo" "b" 1 in
  assert_equal 0 a_offset ; assert_equal 8 b_offset

let suite =
  "SymbolTests"
  >::: [ "test_add_function" >:: test_add_function
       ; "test_find_function" >:: test_find_function
       ; "test_add_multiple_functions" >:: test_add_multiple_functions
       ; "test_add_function_argument" >:: test_add_function_argument
       ; "test_add_local_variable" >:: test_add_local_variable
       ; "test_add_main_function" >:: test_add_main_function
       ; "test_find_main_function" >:: test_find_main_function ]

let () = run_test_tt_main suite
