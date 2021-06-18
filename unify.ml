(** Unification engine used by type inference *)

open Ast

(** Invariant: no TipeVar # on a LHS occurs in any earlier list item. *)
(* TODO: Could we turn this into a hash table for application speed? *)
type substitutions = (int * tipe) list (* TipeVar # * tipe it must be *)

(** A list of types that must unify with each other *)
type constraints = (tipe * tipe) list

(** Return whether a given TipeVar # shows up inside a tipe. If it does, that
    indicates a circular reference. *)
let rec occurs (i : int) (t : tipe) : bool =
  match t with
  | TipeVar j -> i = j
  | FunctionType (args, ret) ->
    List.exists (occurs i) args || occurs i ret
  | _ -> false (* Simple types can't contain references to TipeVars. *)

(** Swap in tipe s for every instance of TipeVar #i in tipe t. *)
let rec subst (s : tipe) (i : int) (t : tipe) : tipe =
  match t with
  | TipeVar y -> if y = i then s else t
  | FunctionType (args, ret) ->
    FunctionType (List.map (subst s i) args, subst s i ret)
  | _ -> t

(** Apply all substitutions in s to t. Loop through s from right to left,
    applying each substitution in turn to t. *)
let apply (s : substitutions) (t : tipe) : tipe =
  List.fold_right (fun (cur_s, t_so_far) -> subst t_so_far cur_s) s t

(** Unify one pair. *)
let rec unify_one (s : tipe) (t : tipe) : substitutions =
  match (s, t) with
  | (TipeVar x, TipeVar y) -> if x = y then [] else [(x, t)]
  | (FunctionType (args1, ret1), FunctionType (args2, ret2)) ->
    if List.compare_lengths args1 args2 = 0
    then unify ((ret1, ret2) :: (Utils.zip args1 args2))
    else failwith "not unifiable: different-length arg lists"
  | ((TipeVar x, z) | (z, TipeVar x)) ->
    if occurs x z
    then failwith "not unifiable: circularity"
    else [(x, z)]
  | (x, y) -> (* unparametrized types like BoolType or IntType or even StringTypes of given lengths *)
    if x = y
    then []
    else failwith "not unifiable: mismatched simple types"

(** Unify a list of constraints: pairs of types that must be equivalent. The
    point is to replace all (or at least most, if there's polymorphism) of the
    TipeVars with concrete types. *)
and unify (pairs : constraints) : substitutions =
  match pairs with
  | [] -> []
  | (x, y) :: rest ->
    let more_subs = unify rest in (* Doesn't this immediately blow up the stack? Could we reverse `pairs` and then go at it tail-recursively instead? (We'd replace fold_right with fold_left, probably, as well, and reverse the invariant.) *)

    (* This concretizes x and y as much as possible given the info gleaned from
       the pairs to the right. This is what maintains the `substitutions`
       invariant: any substitution of a tipevar to the right is applied to
       everything to the left, making those same tipevars go away in leftward
       pairs. Polymorphism is still fine, because the invariant applies only to
       rightward LHSs (which always have substitutions, even if just by another
       tipevar), not RHSs. *)
    let subs = unify_one (apply more_subs x) (apply more_subs y) in

    subs @ more_subs

(* TODO: replace "TipeVar" with "TypeVar". All the rest of the tipe variants say "Type". *)