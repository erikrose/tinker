(* expr - Base type for all expression nodes. *)
type expr =
  (* variant for numeric literals like "1.0". *)
  | Number of float
  | Call of string * expr array
  | String of string

type tipe =
  | NumberType
  | StringType of int (* length *)
  | StringPtrType
  | VoidType

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto = Prototype of string * tipe array

(* func - This is a function that has an implementation in this module. *)
type func = Function of proto * expr
