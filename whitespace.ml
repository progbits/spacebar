(* Lexical tokens *)
type token = Space | Tab | LineFeed | EOF

(* Heap access instructions *)
type heap_acces = Store | Retrieve

(* Flow control instructions *)
type flow_control =
  | Mark of string
  | Call of string
  | UnconditionalJump of string
  | JumpZero of string
  | JumpNegative of string
  | EndSubroutine
  | EndProgram

(* Arithmetic instructions *)
type arithmetic = Addtion | Subtraction | Multiplication | Division | Modulo

(* Stack manipulation instructions *)
type stack_manipulation = Push of int | Duplicate | Swap | Discard

(* IO instructions *)
type io = OutputCharacter | OutputNumber | ReadCharacter | ReadNumber

(* Instruction modification parameters *)
type imp =
  | StackManipulation of stack_manipulation
  | Arithmetic of arithmetic
  | HeapAccess of heap_acces
  | FlowControl of flow_control
  | IO of io

(* Encode a number and return its representation as a string. *)
let encode_number number =
  let rec do_encode number result =
    match number with
    | x when x > 0 -> (
      match x mod 2 with
      | 1 -> do_encode (number / 2) ("\t" ^ result)
      | 0 -> do_encode (number / 2) (" " ^ result)
      | _ -> raise (Failure "This shouldn't happen...") )
    | _ -> result ^ "\n"
  in
  (* Encode number. *)
  let result = do_encode number "" in
  (* Encode Sign. *)
  if number < 0 then "\t" ^ result else " " ^ result

(* Print the representation of an instruction modification parameter to
   `output`. *)
let print output imp =
  match imp with
  | StackManipulation x -> (
    match x with
    | Push x' -> Printf.fprintf output "  %s" (encode_number x')
    | Duplicate -> Printf.fprintf output " \n "
    | Swap -> Printf.fprintf output " \n\t"
    | Discard -> Printf.fprintf output " \n\n" )
  | Arithmetic x -> (
    match x with
    | Addtion -> Printf.fprintf output "\t   "
    | Subtraction -> Printf.fprintf output "\t  \t"
    | Multiplication -> Printf.fprintf output "\t  \n"
    | Division -> Printf.fprintf output "\t \t "
    | Modulo -> Printf.fprintf output "\t \t\t" )
  | HeapAccess x -> (
    match x with
    | Store -> Printf.fprintf output "\t\t "
    | Retrieve -> Printf.fprintf output "\t\t\t" )
  | FlowControl x -> (
    match x with
    | Mark x' -> Printf.fprintf output "\n  %s" x'
    | Call x' -> Printf.fprintf output "\n \t%s" x'
    | UnconditionalJump x' -> Printf.fprintf output "\n \n%s" x'
    | JumpZero x' -> Printf.fprintf output "\n\t %s" x'
    | JumpNegative x' -> Printf.fprintf output "\n\t\t%s" x'
    | EndSubroutine -> Printf.fprintf output "\n\t\n"
    | EndProgram -> Printf.fprintf output "\n\n\n" )
  | IO x -> (
    match x with
    | OutputCharacter -> Printf.fprintf output "\t\n  "
    | OutputNumber -> Printf.fprintf output "\t\n \t"
    | ReadCharacter -> Printf.fprintf output "\t\n\t "
    | ReadNumber -> Printf.fprintf output "\t\n\t\t" )
