type label = {name: string; target: int}

(* Lexical tokens *)
type token = Space | Tab | LineFeed | EOF

(* Heap access instructions *)
type heap_acces = Store | Retrieve

(* Flow control instructions *)
type flow_control =
  | Mark of label
  | Call of label
  | UnconditionalJump of label
  | JumpZero of label
  | JumpNegative of label
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
