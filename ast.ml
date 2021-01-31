type tipe =
  | DoubleType
  | IntType
  | StringType of int (** length *)
  | StringPtrType
  | VoidType
  | FunctionType of tipe list * tipe (** args, return *) 

(** Base type for all expression nodes *)
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
  | If of expr * expr * expr (** condition, then, else *)
  | Var of string (** var read *)
  | Assignment of string * expr (** var write: name, value *)

  (** A function, either internal linkage (with body) or external (without).
      We'll keep declaring types for external functions, even under type
      inference. I don't want to infer signatures from callsites and get it
      wrong, causing the callee to receive garbage and crash. *)
  | Function of string * tipe array * tipe * function_definition (* name, args, return, body *)
  (* Syntax should be something like `fun (a, b) -> external`, where "external"
     is a lexer artifact like "pass" in Python. *)
  (* TODO: We don't strictly need to name functions; we could just assign them to vars (if we had top-level vars). The only problem would be how to know what to call them if we tried to call them from C. It might be tricky to ascertain that statically; what if we had a single function assigned to 2 separate vars? Stick a func ptr in one? (How do we make sure it gets dereffed?) Introduce some `export` syntax? *)
and function_definition =
  | Internal of expr (** function body *)
  | External (** It's found in another module. *)


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
    | Double _
    | Int _
    | String _
    | Function _ -> String_set.empty
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
