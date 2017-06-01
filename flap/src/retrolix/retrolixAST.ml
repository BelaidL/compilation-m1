(** The abstract syntax tree for retrolix programs. *)

type literal =
  | LInt of Int32.t
  | LFun of function_identifier
  | LChar of char
  | LString of string

and identifier = Id of string

and label = Label of string

and function_identifier = FId of string

type register = RId of string

type lvalue = [ `Variable of identifier | `Register of register ]

type rvalue = [ lvalue | `Immediate of literal ]

type t = definition list

and definition =
  (** DValue (x, b) is a block [b] that defines a global variable [x]. *)
  | DValue     of identifier * block
  (** DFunction (f, xs, ys, b) is a function definition with formal
      parameters [xs], and block [b]. *)
  | DFunction  of function_identifier * identifier list * block
  | DExternalFunction of function_identifier

and block =
    (** a block consists in a list of local variables and a list of
        instructions. *)
    identifier list * labelled_instruction list

and labelled_instruction =
    label * instruction

and instruction =
  (** l ← call r (r1, ⋯, rN) *)
  | Call of lvalue * rvalue * rvalue list
  (** tailcall r (r1, ⋯, rN) *)
  | TailCall of rvalue * rvalue list
  (** ret r *)
  | Ret of rvalue
  (** l ← op r1, ⋯, rN *)
  | Assign of lvalue * op * rvalue list
  (** jump ℓ *)
  | Jump of label
  (** jumpif condition r1, r2 → ℓ1, ℓ2 *)
  | ConditionalJump of condition * rvalue list * label * label
  (** switch r -> l1, ..., lN orelse l. *)
  | Switch of rvalue * label array * label option
  (** ;; comment *)
  | Comment of string
  (** exit *)
  | Exit

and op =
  | Load
  | Add | Mul | Div | Sub
  | And | Or
  | Bool of condition

and condition =
  | GT | LT | GTE | LTE | EQ

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)
module IdMap = Map.Make (IdCmp)

(**

  In Retrolix, the toplevel value declarations define global
  variables. The identifiers of these variables must be distinct.

*)
exception GlobalIdentifiersMustBeUnique of identifier

let globals =
  List.fold_left (fun globals -> function
      | DValue (x, _) ->
         if IdSet.mem x globals then
           raise (GlobalIdentifiersMustBeUnique x);
         IdSet.add x globals
      | _ ->
         globals
  ) IdSet.empty

(**
   Every function in Retrolix starts with a declaration
   of local variables. So we need a way to compute the
   local variables of some generated code. This is the
   purpose of the next function:
*)

(** [locals globals b] takes a set of variables [globals] and returns
    the variables use in the list of instructions [b] which are not
    in [globals]. *)
let locals globals b =
   failwith "Students! This is your job!"
