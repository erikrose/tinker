(** Type inference *)

open Ast

exception Error of string

let rec last list =
  match list with
  | [] -> raise (Error "Tried to get the last item of an empty list.")
  | [only] -> only
  | _ :: rest -> last rest

(** Apply type vars to the AST, which we can later derive constraints among and
    solve. *)
let annotate (e : expr) : texpr =
  let type_var_id = ref 0 in
  (** Return a new type variable, identified by a serial number, starting at 1. *)
  let new_type_var () =
    incr type_var_id;
    TipeVar !type_var_id in
  (** Free vars are any vars that we didn't see in any surrounding function
      definitions. So if you have just the expr "foo", foo is a free var. See,
      we can assume that any var seen in a surrounding function definition will
      be *bound* at runtime. We keep a hash of free vars just so we don't keep
      assigning fresh type vars to one every time we see a mention of it. A
      bound var will shadow a free var while the bound var is in scope. If we
      had Python-3-style list comps or OCaml-style let expressions, those would
      introduce new bound vars too. *)
  let free_vars = Hashtbl.create ~random:true 16 in
  (** Return a typed version of the given expression, given the currently in-
      scope variables and their types. *)
  let rec annotate_core (bound_vars : (string * tipe) list) (e : expr) : texpr =
    match e with
    | Bool value -> TBool value
    | Double value -> TDouble value
    | Int value -> TInt value
    | String value -> TString (value, StringType (String.length value))
    | Call (func_expr, args) ->
      (* A call could name a function directly, or it could reference a var (if
         those 2 are even different in our language). So we can't always look
         up the type of the function here. We leave that to the unifier. *)
      let func = annotate_core bound_vars func_expr in
      TCall (func, List.map (annotate_core bound_vars) args, new_type_var ())
      (* Later, collect() will construct a constraint insisting that tipe_of
         func = Function (tipes_of args, that new type var). That should bind
         the new type var to the type of the function body, and then away we
         go. *)
    | Block exprs ->
      let texprs = List.map (annotate_core bound_vars) exprs in
      TBlock (texprs, tipe_of (last texprs))
    | If (if_, then_, else_) ->
      let tthen = annotate_core bound_vars then_ in
      TIf (annotate_core bound_vars if_,
           tthen,
           annotate_core bound_vars else_,
           tipe_of tthen)
      (* We say the typevar that gets assigned to the Then clause is also the
         type of the whole If. We could also assign a fresh one and make
         collect() assert this equivalency instead, just as it will have to for
         the Else clause. It's pretty arbitrary, but maybe it saves a little
         unifier work. *)
    | Var name ->
      begin
        try
          let t = List.assoc name bound_vars in TVar (name, t)
        with Not_found ->
          try
            let t = Hashtbl.find free_vars name in TVar (name, t)
          with Not_found ->
            let t = new_type_var () in Hashtbl.add free_vars name t; TVar (name, t)
      end
    | Assignment (var_name, value, _) ->
      let tvalue = annotate_core bound_vars value in
      TAssignment (var_name, tvalue, tipe_of tvalue)
    | Function (name, arg_name_array, body) ->
      let arg_names = Array.to_list arg_name_array in
      (* Let each arg_name be a new type var: *)
      let arg_tipes = List.map (fun _ -> new_type_var ()) arg_names in
      let arg_names_and_tipes = List.map2 (fun n t  -> (n, t)) arg_names arg_tipes in
      let tbody = annotate_core (arg_names_and_tipes @ bound_vars) body in (* TODO: Add function name itself to scope. Or dispense with functions having names, and figure out how to get main() called (and, later, how to get Tinker functions linkable from other languages). *)
      TFunction (name, arg_name_array, tbody, FunctionType (arg_tipes, tipe_of tbody))
    | ExternalFunction (name, function_tipe) ->
      TExternalFunction (name, function_tipe)
  in annotate_core [] e

(** Given a list of typed expressions, return a list of pairs to unify. This
    function is where the typing rules are applied (or at least the ones not
    already applied in annotate()). *)
let rec collect (texprs : texpr list) (unifying_pairs : (tipe * tipe) list) : (tipe * tipe) list =
  (* Basically, we call collect() recursively, adding any contained texprs that
     might provide material for further constraints to the first param and
     building the list of constraints in the second. *)
  match texprs with
  | [] -> unifying_pairs
  | (TBool _ | TDouble _ | TInt _ | TString _ | TVar _ | TExternalFunction _) :: rest ->
    (* There are no constraints to extract from literals and these other things. *)
    collect rest unifying_pairs
  | TCall (func, args, ret_tipe) :: rest ->
    (* Make sure the type of the function agrees with the types of the args
       passed in and the return type expected. *)
    collect (func :: args @ rest) ((tipe_of func, FunctionType ((List.map tipe_of args), ret_tipe)) :: unifying_pairs)
  | TBlock (block_texprs, _) :: rest ->
    (* The typing rule "the type of a block is the type of its last statement"
       is already applied by annotate(). *)
    collect (block_texprs @ rest) unifying_pairs
  | TIf (if_, then_, else_, tipe_) :: rest ->
    collect (if_ :: then_ :: else_ :: rest)
            ((* `if` conditions are bools: *)
             (tipe_of if_, BoolType) ::
             (* The `then` and `else` clauses return the same type: *)
             (tipe_of then_, tipe_of else_) ::
             (* And the `then` and `else` tipes are the same as the tipe of the
                whole `if` expression: *)
             (tipe_of then_, tipe_) ::
             unifying_pairs)
  | TAssignment (_, rvalue, _) :: rest ->
    (* The typing rule "an assigned var has the type of its rvalue" is applied by annotate(). *)
    collect (rvalue :: rest) unifying_pairs
  | TFunction (_, _, body, _) :: rest ->
    (* Arg names are already unified with occurrences of them in the function body, via annotate. *)
    collect (body :: rest) unifying_pairs

(*
let unify e =
  let annotated = annotate e in
  let constraints = collect [annotated] in
  let resolutions = unify constraints in
  apply resolutions annotated (or something)
*)