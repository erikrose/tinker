type tipe =
  | BoolType
  | DoubleType
  | IntType
  | StringType of int (** length. TODO: Make all strings the same type *)
  | StringPtrType
  | VoidType
  | FunctionType of tipe list * tipe (** args, return *)
  | TipeVar of int (** serial number, starting from 0 *)
  [@@deriving show]

(** Base type for all expression nodes *)
type expr =
  (* TODO: Add line and col to each variant for error reporting. Turn
      everything into records, because that'll make them pretty long. *)
  | Bool of bool
  | Double of float
  | Int of int (** semantic: 64-bit int *)
  | String of string
  | Call of expr * expr list (** function expr, args *)

  (** A block is a sequence of expressions whose value is that of the last
      expression evaluated. This exists as a separate idea from functions
      because it might also occur in global namespace. Besides, it gives us the
      flexibility to become block-scoped at some point. An empty block is an
      error, because what type would it be? *)
  | Block of expr list
  | If of expr * expr * expr (** condition, then, else *)
  | Var of string (** var read *)
  | Assignment of string * expr (** var write: name, value *)

  (** A function definition *)
  | Function of string * string array * expr (* name, args, body *)
  (* TODO: We don't strictly need to name functions; we could just assign them to vars (if we had top-level vars). The only problem would be how to know what to call them if we tried to call them from C. Introduce some `export` syntax? *)
  
  (** A declaration of an external function. We keep declaring types for these,
      even under type inference. I don't want to infer signatures from callsites
      and get it wrong, causing the callee to receive garbage and crash. *)
  | ExternalFunction of string * tipe (* name, FunctionType *)
  (* Syntax might be something like `fun (a, b) -> external`, where "external"
     is a lexer artifact like "pass" in Python. *)

(** Type-annotated expressions *)
type texpr =
  | TBool of bool
  | TDouble of float
  | TInt of int
  | TString of string * tipe (* string literal *)
  | TCall of texpr * texpr list * tipe (* func, args, return tipe *)
  | TBlock of texpr list * tipe
  | TIf of texpr * texpr * texpr * tipe
  | TVar of string * tipe
  | TAssignment of string * texpr * tipe
  | TFunction of string * string array * texpr * tipe (* name, arg names, body, FunctionType *)
  | TExternalFunction of string * tipe
  [@@deriving show]

let tipe_of texp =
  match texp with
  | TBool _ -> BoolType
  | TDouble _ -> DoubleType
  | TInt _ -> IntType
  | TString (_, tipe) -> tipe
  | TCall (_, _, ret_tipe) -> ret_tipe
  | TBlock (_, tipe) -> tipe
  | TIf (_, _, _, tipe) -> tipe
  | TVar (_, tipe) -> tipe
  | TAssignment (_, _, tipe) -> tipe
  | TFunction (_, _, _, function_type) -> function_type
  | TExternalFunction (_, tipe) -> tipe
 
module String_set = Set.Make (String)

(** Raise an exception if there is a var that could be read before being
    written in the given expression. We do not recurse into further Functions
    because those are different scopes.
    @raise exc.UndefinedVar if we encounter a var read before it's written *)
let assert_no_unwritten_reads_in_scope exp =
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
    | Bool _
    | Double _
    | Int _
    | String _
    | Function _
    | ExternalFunction _ -> String_set.empty
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
