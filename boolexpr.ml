(** Boolean expressions *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

open Format;;

(** Type of a Boolean expression, parametrized by the type of atomic conditions
*)
type 'cond t =
  | CONDITION of 'cond
      (** Atomic condition *)
  | RANDOM
      (** Unknown condition (should appear only at the top level *)
  | CST of bool
      (** Constant atom (true or false) *)
  | OR of 'cond t * 'cond t
      (** Disjunction *)
  | AND of 'cond t * 'cond t
      (** Conjunction *)

(** Printing function, parametrized by a printing function for atomic
    conditions *)
let rec print (printa:Format.formatter -> 'a -> unit) (fmt:Format.formatter) (co:'a t) : unit =
  match co with
  | CONDITION co -> printa fmt co
  | CST b -> if b then pp_print_string fmt "true" else pp_print_string fmt "false"
  | RANDOM -> pp_print_string fmt "?"
  | OR (c1,c2) ->
      fprintf fmt "(%a || %a)"
      (print printa) c1 (print printa) c2
  | AND (c1,c2) ->
      fprintf fmt "(%a && %a)"
      (print printa) c1 (print printa) c2

(** Map-iterator, based on an atomic condition transformer *)
let rec map (f:'a -> 'b) (expr:'a t) : 'b t =
  match expr with
  | RANDOM -> RANDOM
  | CST x -> CST x
  | CONDITION c -> CONDITION(f c)
  | OR(a,b) -> OR(map f a, map f b)
  | AND(a,b) -> AND(map f a, map f b)


(*  ********************************************************************* *)
(** {2 Constructors for Boolean expressions} *)
(*  ********************************************************************* *)

(** Some of these constructors may simplify the resulting expressions. *)

let make_condition (co:'a) =
  CONDITION co

let make_cst (bo:bool) =
  CST bo

let make_or (c1:'a t) (c2:'a t) =
  begin match c1 with
  | CST(b) -> if b then c1 else c2
  | _ ->
      begin match c2 with
      | CST(b) -> if b then c2 else c1
      | _ -> OR (c1,c2)
      end
  end

let make_and (c1:'a t) (c2:'a t) =
  begin match c1 with
  | CST(b) -> if b then c2 else c1
  | _ ->
      begin match c2 with
      | CST(b) -> if b then c1 else c2
      | _ -> AND (c1,c2)
      end
  end

(** Negation, parametrized by a negation function for atomic conditions *)
let rec make_not (negate:'a -> 'a) (expr:'a t) : 'a t =
  match expr with
  | CONDITION co -> CONDITION(negate co)
  | CST b -> CST (not b)
  | RANDOM -> RANDOM
  | OR(c1,c2) -> make_and (make_not negate c1) (make_not negate c2)
  | AND(c1,c2) -> make_or (make_not negate c1) (make_not negate c2)
