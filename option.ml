(**  Argument, Options and Parsing of command line} *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

(** input filename *)
let inputfilename = ref ""

(** debug level *)
let debug = ref 0

(*  ---------------------------------------------------------------------- *)
(** {3 Display style} *)
(*  ---------------------------------------------------------------------- *)

type displaytags = {
  precolorB : string;
  precolorR : string;
  precolorG : string;
  postcolor : string;
}
let (texttags:displaytags) = {
  precolorB=""; 
  precolorR=""; 
  precolorG=""; 
  postcolor="";
}
let (colortags:displaytags) = {
  precolorB ="\027[32m";
  precolorR = "\027[31m"; 
  precolorG="\027[34m";
  postcolor="\027[m";
}
let (htmltags:displaytags) = {
  precolorB ="<span style=\"color:blue;\">";
  precolorR ="<span style=\"color:red;\">";
  precolorG="<span style=\"color:green;\">";
  postcolor="</span>";
}

let (assocnamedisplaytags : (string * displaytags) list) =
  [
    ("text",texttags);
    ("color",colortags);
    ("html",htmltags);
  ]
let (lnamedisplaytags : string list) =
  List.map
    (fun (name,_) -> name)
    assocnamedisplaytags

let displaytags = ref colortags

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of abstract domain} *)
(*  ---------------------------------------------------------------------- *)
type domain =
| Box
| Octagon
| PolkaLoose
| PolkaStrict
| PolkaEq
| PplPolyLoose
| PplPolyStrict
| PplGrid
| PolkaGrid
let (assocnamedomain : (string * domain) list) =
  [
    ("box",Box);
    ("octagon",Octagon);
    ("polka",PolkaLoose);
    ("polkastrict",PolkaStrict);
    ("polkaeq",PolkaEq);
    ("ppl",PplPolyLoose);
    ("pplstrict",PplPolyStrict);
    ("pplgrid",PplGrid);
    ("polkagrid",PolkaGrid)
  ]
let (lnamedomain : string list) =
  List.map
    (fun (name,_) -> name)
    assocnamedomain
let domain = ref PolkaLoose (** abstract domain to use *)

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of analysis type} *)
(*  ---------------------------------------------------------------------- *)
type analysis =
  | Forward
  | Backward
let analysis = ref [Forward]

(*  ---------------------------------------------------------------------- *)
(** {3 Main specification list} *)
(*  ---------------------------------------------------------------------- *)

let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  Arg.align
    [
      (
	"-debug",
	Arg.Set_int(debug),
	"<int> : debug level, from 0 to 4 (default:0)"
      );
      (
	"-domain",
	Arg.Symbol(
	  lnamedomain,
	  begin fun name -> domain := List.assoc name assocnamedomain end
	),
	" : abstract domain to use (default: polka)"
      );
      (
	"-depth",
	Arg.Int(begin fun n ->
	  if n<2 then
	    raise (Arg.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-depth' expects an integer >= 2"))
	  else
	    Solving.iteration_depth := n
	end),
	"<int> : depth of recursive iterations (default 2, may only be more)"
      );
      (
	"-guided",
	Arg.Bool(begin fun b -> Solving.iteration_guided := b end),
	"<bool> : if true, guided analysis of Gopand and Reps (default: false)"
      );
      (
	"-widening",
	Arg.Tuple([
	  Arg.Bool(fun b -> Solving.widening_first := b);
	  Arg.Int(begin fun n ->
	    if n<0 then
	      raise (Arg.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `widening start' argument"))
	    else
	      Solving.widening_start := n
	  end);
	  Arg.Int(begin fun n ->
	    if n<1 then
	      raise (Arg.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a stricly positive integer for its `widening frequency' argument"))
	    else
	      Solving.widening_freq := n
	  end);
	  Arg.Int(begin fun n ->
	    if n<0 then
	      raise (Arg.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `descending' argument"))
	    else
	      Solving.widening_descend := n
	  end)
	]),
	"<bool><int><int><int> : specifies usage of widening first heuristics, delay and frequency of widening, and nb. of descending steps (default: false 1 1 2)"
      )
      ;
      (
	"-display",
	Arg.Symbol(
	  lnamedisplaytags,
	  begin fun name -> displaytags := List.assoc name assocnamedisplaytags end
	),
	" : display style (default: color)"
      );
      (
	"-margin",
	Arg.Int(fun n -> Format.set_margin n),
	" : nb of columns for text display"
      );
      (
	"-analysis",
	Arg.String(begin fun str ->
	  analysis := [];
	  String.iter
	    (begin fun chr ->
	      match chr with
	      | 'f' ->
		  analysis := Forward :: !analysis
	      | 'b' ->
		  analysis := Backward :: !analysis
	      | _ ->
		  raise (Arg.Bad ("Wrong argument `"^str^"'; option `-analysis' expects only 'f' or 'b' characters in its argument string"))
	    end)
	    str;
	  analysis := List.rev !analysis;
	  if !analysis=[] then analysis := [Forward];
	end),
	"<('f'|'b')+> : sequence of forward and backward analyses to perform (default \"f\")"
      )
    ]
