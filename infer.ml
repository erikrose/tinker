(** Type inference *)

open Ast

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
      TBlock (texprs, tipe_of (Utils.last texprs))
      (* TODO: Raise a nicer exception if a block is empty. Maybe move that to the parser when it exists. *)
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
          try (* TODO: Go look for functions if you don't otherwise find a var, like codegen does. Or insert functions into the namespace properly. *)
            let t = Hashtbl.find free_vars name in TVar (name, t)
          with Not_found ->
            let t = new_type_var () in
            Hashtbl.add free_vars name t; TVar (name, t)
      end
    | Assignment (var_name, value) ->
      let right_type = annotate_core bound_vars value in
      (* Apply the typing rule "Assignments have the type of their lvalue's
         var."

         Here in annotate(), we say an assignment's type is whatever its lvalue
         var's is, not whatever its rvalue's is. The declaration of equivalence
         between the types is done later by collect(). This serves to express
         equivalences between reads and writes to a var; they end up both being
         TipeVar 7 rather than the Var being a TipeVar and the Assignment `x =
         3` being, for example, IntType. *)
      let left_type = List.assoc var_name bound_vars in
      TAssignment (var_name, right_type, left_type)
    | Function (name, arg_name_array, body) ->
      let arg_names = Array.to_list arg_name_array in
      (* Let each arg_name be a new type var: *)
      let arg_tipes = List.map (fun _ -> new_type_var ()) arg_names in
      let arg_names_and_tipes = Utils.zip arg_names arg_tipes in
      (* Also add all this function's local vars to the bound_vars. Locals are
         just like args with no incoming value; they can push on and pop off at
         the same time: *)
      let assigned_vars_and_tipes = List.map (fun name -> (name, new_type_var ())) (assignments_in body) in
      let tbody = annotate_core (arg_names_and_tipes @ assigned_vars_and_tipes @ bound_vars) body in (* TODO: Add function name itself to scope. Or dispense with functions having names, and figure out how to get main() called (and, later, how to get Tinker functions linkable from other languages). *)
      TFunction (name, arg_name_array, tbody, FunctionType (arg_tipes, tipe_of tbody))
    | ExternalFunction (name, function_tipe) ->
      TExternalFunction (name, function_tipe)
  in annotate_core [] e

(** Given a list of typed expressions, return a list of pairs that must unify.
    This function is where most of the typing rules are applied--the ones not
    already applied in annotate(). *)
let rec collect (texprs : texpr list) (unifying_pairs : Unify.constraints) : Unify.constraints =
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
  | TAssignment (_, rvalue, var_type) :: rest ->
    (* Apply the typing rule "an assigned var has the type of its rvalue": *)
    collect (rvalue :: rest) ((tipe_of rvalue, var_type) :: unifying_pairs)
  | TFunction (_, _, body, _) :: rest ->
    (* Arg names are already unified with occurrences of them in the function
       body, via annotate. *)
    collect (body :: rest) unifying_pairs

(** Apply substitutions from the unifier to the types hanging off an annotated
    expression tree, making concrete all the types we can. (The rest will have
    to be monomorphized or whatever.) *)
let rec substitute (s : Unify.substitutions) (te : texpr) : texpr =
  match te with
  | TCall (func, args, ret_tipe) ->
    TCall (substitute s func,
           List.map (substitute s) args,
           Unify.apply s ret_tipe) (* We can't stop after we hit the first match because we could be replacing TipeVar 3 with FunctionType (..., TipeVar 7), which could then need TipeVar 7 replaced more leftward in s. *)
  | TBlock (texprs, t) ->
    TBlock (List.map (substitute s) texprs, Unify.apply s t)
  | TIf (if_, then_, else_, t) ->
    TIf (substitute s if_,
         substitute s then_,
         substitute s else_,
         Unify.apply s t)
  | TVar (name, t) ->
    TVar (name, Unify.apply s t)
  | TAssignment (var_name, rvalue, t) ->
    TAssignment (var_name, substitute s rvalue, Unify.apply s t)
  | TFunction (name, arg_name_array, body, t) ->
    TFunction (name, arg_name_array, substitute s body, Unify.apply s t)
  | TExternalFunction (name, t) ->
    TExternalFunction (name, Unify.apply s t)
  | TBool _ | TDouble _ | TInt _ | TString _ -> te

(** Use inference to turn an expression into a typed one. *)
let infer_types (e : expr) : texpr =
  let annotated = annotate e in
  let constraints = collect [annotated] [] in
  let substitutions = Unify.unify constraints in
  substitute substitutions annotated

(* TODO: Think about turning substitutions into a hash for speed. Meditate on whether it would be any faster. I think many things are O(|nodes in the texpr|) plus a little for recursing through the tipe (but I think those are typically gonna be very shallow). *)
