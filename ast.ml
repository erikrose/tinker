(* Base type for all expression nodes *)
type expr =
  | Double of float
  | Int of int (** semantic: 64-bit int *)
  | String of string
  | Call of string * expr list (** name, args *)

  (** A block is a sequence of expressions whose value is that of the last
      expression evaluated. This exists as a separate idea from functions
      because it might also occur in global namespace. Besides, it gives us the
      flexibility to become block-scoped at some point. *)
  | Block of expr list
  | If of expr * expr * expr (* condition, then, else *)
  | Var of string (* var read *)
  | Assignment of string * expr (* var write: name, value *)
  (* TODO: Why aren't functions and protos expressions? It would be nice for
     them to be. Then everything could have a type. *)

type tipe =
  | DoubleType
  | IntType
  | StringType of int (* length *)
  | StringPtrType
  | VoidType

(** The "prototype" for a function, which captures its name, argument types, and
    return type *)
type proto = Prototype of string * tipe array * tipe

(** A function that has an implementation in this module. *)
type func = Function of proto * expr

module String_set = Set.Make (String)

(** Raise an exception if there is a var that could be read before being
    written in the given expression.
    @raise exc.UndefinedVar if we encounter a var read before it's written *)
let assert_no_unwritten_reads_in exp =
  (** Return the set of vars we can prove has been written in the expression.
      @param written The set of variables we can prove have been written to
        already
      @raise exc.UndefinedVar if we encounter a var read before it's written

      Because this language's CFG mirrors its AST, we can just operate over the
      AST. But if we ever grow early returns (unless they lower to early returns
      in LLVM IR), break statements, or continue statements, we'll have to build
      a CFG and operate over that instead. *)
  let rec proven_written exp written =
    match exp with
    | Double _
    | Int _
    | String _ -> String_set.empty
    | If (if_, then_, else_) ->
      let written_in_if = proven_written if_ written in
      let written_in_then = proven_written then_ written_in_if in
      let written_in_else = proven_written else_ written_in_if in
      (* Optimize: no need to return the entire pre-existing set of vars from
         the then and else blocks; we end up intersecting more than just the new
         vars from those blocks. *)
      String_set.inter written_in_then written_in_else
    | Var name ->
      if String_set.mem name written then
        written
      else
        raise (Exc.Undefined_var name)
    | Call (_, args) ->
      (* Evaluate each arg in the context of the vars provably written in
         previous args: *)
      List.fold_left (fun accum exp -> proven_written exp accum) written args
    | Assignment (var_name, _) ->
      String_set.add var_name written
    | Block exprs ->
      (* For each expr in the block, evaluate each in the context of the vars
         provably written in the previous exprs: *)
      let all_written_vars accum cur_expr =
        proven_written cur_expr accum in
      List.fold_left all_written_vars written exprs
  in
  ignore (proven_written exp String_set.empty)
