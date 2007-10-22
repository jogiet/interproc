(** Boolean expressions *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

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

val print :
  (Format.formatter -> 'cond -> unit) -> Format.formatter -> 'cond t -> unit
  (** Printing function, parametrized by a printing function for atomic
    conditions *)

val map : ('conda -> 'condb) -> 'conda t -> 'condb t
  (** Map-iterator, based on an atomic condition transformer *)

(*  ********************************************************************* *)
(** {2 Constructors for Boolean expressions} *)
(*  ********************************************************************* *)

(** Some of these constructors may simplify the resulting expressions. *)

val make_condition : 'cond -> 'cond t
val make_cst : bool -> 'cond t
val make_or : 'cond t -> 'cond t -> 'cond t
val make_and : 'cond t -> 'cond t -> 'cond t
val make_not : ('cond -> 'cond) -> 'cond t -> 'cond t
  (** Negation, parametrized by a negation function for atomic conditions *)
