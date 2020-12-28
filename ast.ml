(* Base type for all expression nodes *)
type expr =
  | Double of float
  | Int of int (* semantic: 64-bit int *)
  | Call of string * expr array
  | String of string
  | Block of expr array (* A block is a sequence of expressions whose value is that of the last expression evaluated. This exists as a separate idea from functions because it might also occur in global namespace. Besides, it gives us the flexibility to become block-scoped at some point. *) (* TODO: Probably turn this into a list once we quit using AST literals. *)
  | If of expr * expr * expr
  (* TODO: Why aren't functions and protos expressions? It would be nice for
     them to be. Then everything could have a type. *)

type tipe =
  | DoubleType
  | IntType
  | StringType of int (* length *)
  | StringPtrType
  | VoidType

(* The "prototype" for a function, which captures its name, argument types, and
   return type *)
type proto = Prototype of string * tipe array * tipe

(* A function that has an implementation in this module. *)
type func = Function of proto * expr
