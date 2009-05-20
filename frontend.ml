(** *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

open Format
open Option

(*  ********************************************************************** *)
(** {2 Parsing input file} *)
(*  ********************************************************************** *)

let parse_lexbuf
  (fmt:Format.formatter)
  (lexbuf:Lexing.lexbuf)
  : 
  Spl_syn.program
  =
  let prog =
    try
      Spl_yacc.program Spl_lex.token lexbuf
    with
    | Parsing.Parse_error ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf fmt
	  "Syntaxical error: %s, line %i, characters %i-%i: '%s'.@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf);
	raise Exit;
    | Spl_syn.Error ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf fmt "Lexical error: %s, line %i, character %i-%i: '%s'.@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf);
	raise Exit
  in
  prog

(*  ********************************************************************** *)
(** {2 Analyzing and displaying the solution} *)
(*  ********************************************************************** *)

let build_graphs
  (prog:Spl_syn.program)
  :
  Equation.graph * Equation.graph
  =
  (* Converting prog into a forward equation system *)
  let (fgraph:Equation.graph) = Syn2equation.Forward.make prog in
  if !debug>0 then
    printf "%sForward equation graph%s@   @[<v>%a@]@."
      (!Option.displaytags).precolorB (!Option.displaytags).postcolor
      (PSHGraph.print
	PSpl_syn.print_point
	pp_print_int
	(fun fmt () -> pp_print_string fmt "()")
	Equation.print_transfer
	Equation.print_info)
      fgraph
  ;
  (* Converting prog into a backward equation system *)
  let (bgraph:Equation.graph) = Syn2equation.Backward.make prog in
  if !debug>0 then
    printf "%sBackward equation graph%s@   @[<v>%a@]@."
      (!Option.displaytags).precolorB (!Option.displaytags).postcolor
      (PSHGraph.print
	PSpl_syn.print_point
	pp_print_int
	(fun fmt () -> pp_print_string fmt "()")
	Equation.print_transfer
	Equation.print_info)
      bgraph
  ;
  (fgraph,bgraph)

let compute_and_display
  (fmt:Format.formatter)
  (prog:Spl_syn.program)
  (fgraph:Equation.graph) (bgraph:Equation.graph)
  (manager:'a Apron.Manager.t)
  :
  unit
  =
  let
    (previous
      :
      (Spl_syn.point, int, 'a Apron.Abstract1.t, unit) Fixpoint.output option ref
    )
    =
    ref None
  in
  List.iter
    (begin fun t ->
      (* Computation *)
      let fp =
	begin match t with
	| Forward ->
	    let fp =
	      Solving.Forward.compute fgraph ~output:(!previous) manager ~debug:!debug
	    in
	    fprintf fmt "%sAnnotated program after forward analysis%s@ "
	      (!Option.displaytags).precolorB (!Option.displaytags).postcolor
	    ;
	    fp
	| Backward ->
	    let fp =
	      Solving.Backward.compute prog bgraph ~output:(!previous) manager ~debug:!debug
	    in
	    fprintf fmt "%sAnnotated program after backward analysis%s@ "
	      (!Option.displaytags).precolorB (!Option.displaytags).postcolor
	    ;
	    fp
	end
      in
      (* Display *)
      fprintf fmt "@[<v>%a@]@."
	(PSpl_syn.print_program
	  begin fun fmt (point:Spl_syn.point) ->
	    let abs = PSHGraph.attrvertex fp point in
	    fprintf fmt "@[<hov>%s%a@ %a%s@]"
	      (!Option.displaytags).precolorR
	      PSpl_syn.print_point point
	      Apron.Abstract1.print abs
	      (!Option.displaytags).postcolor
	  end)
	prog
      ;
      previous := Some fp;
    end)
    !analysis
  ;
  ()

let analyze_and_display
  (fmt:Format.formatter)
  (prog:Spl_syn.program)
  :
  unit
  =
  let (fgraph,bgraph) = build_graphs prog in
  (* Computing solution *)
  begin match !domain with
  | Box ->
      compute_and_display fmt prog fgraph bgraph
      (Box.manager_alloc ())
  | Octagon ->
      compute_and_display fmt prog fgraph bgraph
      (Oct.manager_alloc ()) 
  | PolkaLoose ->
      compute_and_display fmt prog fgraph bgraph
      (Polka.manager_alloc_loose ()) 
  | PolkaStrict ->
      compute_and_display fmt prog fgraph bgraph
      (Polka.manager_alloc_strict ()) 
  | PolkaEq ->
      compute_and_display fmt prog fgraph bgraph
      (Polka.manager_alloc_equalities ()) 
  | PplPolyLoose ->
      compute_and_display fmt prog fgraph bgraph
      (Ppl.manager_alloc_loose ()) 
  | PplPolyStrict ->
      compute_and_display fmt prog fgraph bgraph
      (Ppl.manager_alloc_strict ()) 
  | PplGrid ->
      compute_and_display fmt prog fgraph bgraph
      (Ppl.manager_alloc_grid ()) 
  | PolkaGrid ->
      compute_and_display fmt prog fgraph bgraph
      (PolkaGrid.manager_alloc_loose ()) 
  end


