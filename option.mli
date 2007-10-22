(**  Argument, Options and Parsing of command line *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

val inputfilename : string ref
  (** input filename *)
val debug : int ref
  (** debug level *)

(*  ---------------------------------------------------------------------- *)
(** {3 Display style} *)
(*  ---------------------------------------------------------------------- *)

type displaytags = {
  precolorB : string;
  precolorR : string;
  precolorG : string;
  postcolor : string;
}
val texttags : displaytags
val colortags : displaytags
val htmltags : displaytags

val displaytags : displaytags ref

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of abstract domain} *)
(*  ---------------------------------------------------------------------- *)

type domain =
    Box
  | Octagon
  | PolkaLoose
  | PolkaStrict
  | PolkaEq
  | PplPolyLoose
  | PplPolyStrict
  | PplGrid
  | PolkaGrid
val domain : domain ref

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of analysis type} *)
(*  ---------------------------------------------------------------------- *)

type analysis = Forward | Backward
val analysis : analysis list ref

(*  ---------------------------------------------------------------------- *)
(** {3 Speclist} *)
(*  ---------------------------------------------------------------------- *)

val speclist : (Arg.key * Arg.spec * Arg.doc) list
