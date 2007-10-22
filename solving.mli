(** Solving the equations *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

(*  ********************************************************************* *)
(** {2 Instanciated module and options} *)
(*  ********************************************************************* *)

module Fixpoint : (MkFixpoint.S with module Graph = Equation.Graph)
(** Fixpoint equations solver *)

(*  ===================================================================== *)
(** {3 Options} *)
(*  ===================================================================== *)

val iteration_depth : int ref
  (** Depth of recursion in iteration. If the depth is deeper, one tries to
    stabilize inner loops first before propagating to enclosing loops.*)

val iteration_guided : bool ref
  (** Guided iteration technique *)

val widening_first : bool ref
  (** If true, a newly activated incoming edge disables (for this step)
    widening. *)

val widening_start : int ref
  (** Number of steps without widening *)

val widening_freq : int ref
  (** Widening every x steps *)

val widening_descend : int ref
  (** Number of descending iterations *)

(*  ********************************************************************* *)
(** {2 Forward analysis} *)
(*  ********************************************************************* *)

module Forward : sig
  val apply :
    Equation.graph ->
    output:('a Apron.Abstract1.t, unit) Fixpoint.output option ->
    'a Apron.Manager.t ->
    Equation.Graph.hedge -> 'a Apron.Abstract1.t array ->
    unit * 'a Apron.Abstract1.t
      (** Applying a transfer function, given
	- the equation graph ;
	- optionally, the result of a previous, backward analysis;
	- an APRON manager;
	- an hyperedge identifier;
	- an array of input abstract values
      *)

  val compute :
    Equation.graph ->
    output:('a Apron.Abstract1.t, unit) Fixpoint.output option ->
    'a Apron.Manager.t ->
    debug:int ->
    ('a Apron.Abstract1.t, unit) Fixpoint.output
      (** Compute (post)fixpoint, given
	- the equation graph;
	- optionally, the result of a previous, backward analysis
	- an APRON manager;
	- a debug level
      *)
end

(*  ********************************************************************* *)
(** {2 Backward analysis} *)
(*  ********************************************************************* *)

module Backward : sig
  val apply :
    Equation.graph ->
    output:('a Apron.Abstract1.t, unit) Fixpoint.output option ->
    'a Apron.Manager.t ->
    Equation.Graph.hedge -> 'a Apron.Abstract1.t array ->
    unit * 'a Apron.Abstract1.t
      (** Applying a transfer function *)

  val compute :
    Spl_syn.program ->
    Equation.graph ->
    output:('a Apron.Abstract1.t, unit) Fixpoint.output option ->
    'a Apron.Manager.t ->
    debug:int ->
    ('a Apron.Abstract1.t, unit) Fixpoint.output
      (** Compute (post)fixpoint *)
end
