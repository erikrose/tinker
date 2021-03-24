(** Type inference *)

open Ast

exception Error of string

let rec last list =
  match list with
  | [] -> raise (Error "Tried to get the last item of an empty list.")
  | [only] -> only
  | head :: rest -> last rest

(** Apply type vars to the AST, which we can later derive constraints between
    and solve. *)
let annotate (e : expr) : texpr =
  let type_var_id = ref 0 in
  (** Return a new type variable, identified by a serial number. *)
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
      begin
        match tipe_of func with
        | FunctionType (arg_tipes, ret_type) ->
          TCall (func, List.map (annotate_core bound_vars) args, ret_type)
        | _ -> raise (Error "You tried to call something that isn't a function.")
      end
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
      let arg_tipes = List.map (fun arg -> new_type_var ()) arg_names in
      let arg_names_and_tipes = List.map2 (fun n t  -> (n, t)) arg_names arg_tipes in
      let tbody = annotate_core (arg_names_and_tipes @ bound_vars) body in
      TFunction (name, arg_name_array, tbody, FunctionType (arg_tipes, tipe_of tbody))
    | ExternalFunction (name, function_tipe) ->
      TExternalFunction (name, function_tipe)
  in annotate_core [] e

(*
let rec collect (texprs : texpr list) =
  match  in
  | TIf (if_, then_, else_, tipe_) ->
    yield from [(if_, bool), (then_, else_)]
*)