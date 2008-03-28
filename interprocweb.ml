(** CGI-Interface for the interproc analyzer *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

open Html
open Http
open Cgi
open Sscookie
open Date

(* you will need to change the following address *)
module Link = struct
  let (url:string) =    
    Html.link
      "http://pop-art.inrialpes.fr/interproc/interprocweb.cgi"
      "interprocweb"
  let (apron:string) =
    Html.link
      "http://apron.cri.ensmp.fr/library/"
      "APRON Abstract Domain Library"
  let (fixpoint:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/fixpoint/index.html"
      "Fixpoint Solver Library"
  let (interproc:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/interproc/index.html"
      "Interproc"
  let (simple_syntax:string) =
    Html.link
      "http://bjeannet.gforge.inria.fr/interproc/manual_syntax.html"
      "``Simple'' language syntax"
  let (program_examples:string) =
    Print.sprintf "%s %s %s %s"
      (Html.link 
	"ackerman.txt" "ackerman")
      (Html.link
	"maccarthy91.txt" "maccarthy91")
      (Html.link
	"heapsort.txt" "heapsort")
      (Html.link
	"symmetricalstairs.txt" "symmetricalstairs")

  let (ocamlhtml:string) =
    Html.link
      "http://www.eleves.ens.fr/home/mine/ocamlhtml/"
      "OCamlHtml library"
end

(* ********************************************************************** *)
(* examples *)
(* ********************************************************************** *)

let ackerman = 
"proc ack(x:int,y:int) returns (res:int)\r\n"^
"var t:int, t1:int;\r\n"^
"begin\r\n"^
"  assume x>=0 and y>=0;\r\n"^
"  if (x<=0) then /* x<=0 instead of x==0 (more precise) */\r\n"^
"    res = y+1;\r\n"^
"  else\r\n"^
"    if (y<=0) then /* y<=0 instead of x==0 (more precise) */\r\n"^
"      t1 = x-1;\r\n"^
"      t = 1;\r\n"^
"      res = ack(t1,t);\r\n"^
"    else\r\n"^
"      t1 = y-1;\r\n"^
"      t = ack(x,t1);\r\n"^
"      t1 = x-1;\r\n"^
"      res = ack(t1,t);\r\n"^
"    endif ;\r\n"^
"  endif;\r\n"^
"end\r\n"^
"\r\n"^
"var a:int, b:int, r:int;\r\n"^
"begin\r\n"^
"  r = ack(a,b);\r\n"^
"end\r\n"

let maccarthy91 = 
"/* exact semantics:\r\n"^
"   if (n>=101) then n-10 else 91 */\r\n"^
"proc MC(n:int) returns (r:int)\r\n"^
"var t1:int, t2:int;\r\n"^
"begin\r\n"^
"  if (n>100) then\r\n"^
"     r = n-10;\r\n"^
"  else\r\n"^
"     t1 = n+11;\r\n"^
"     t2 = MC(t1);\r\n"^
"     r = MC(t2);\r\n"^
"  endif;\r\n"^
"end\r\n"^
"\r\n"^
"var a:int, b:int;\r\n"^
"begin\r\n"^
"  b = MC(a);\r\n"^
"end\r\n"

let heapsort = 
"proc div2(a:int) returns (b:int)\r\n"^
"begin\r\n"^
"  assume (a-2*b>=0 and a-2*b<=1); /* trick to encode b = a div 2 */\r\n"^
"end\r\n"^
"\r\n"^
"proc heapsort(N:int) returns (res:int)\r\n"^
"var L:int,R:int,I:int,J:int,\r\n"^
"continue:int,nondet:int;\r\n"^
"begin\r\n"^
"  assume N>=2;\r\n"^
"  L = div2(N);\r\n"^
"  L = L+1;\r\n"^
"  if (L>=2) then\r\n"^
"    L = L-1; /* K = T[L]; */\r\n"^
"  else\r\n"^
"    /* K = T[R]; T[R] = T[1]; */\r\n"^
"    R = R-1;\r\n"^
"  endif;\r\n"^
"  while (R>=2) do\r\n"^
"    I = L;\r\n"^
"    J = 2*I;\r\n"^
"    continue = 1;\r\n"^
"    while (J<=R and continue>0) do\r\n"^
"      if (J<=R-1) then\r\n"^
"	if /* T[J]<T[j-1] */ brandom then\r\n"^
"	  J = J+1;\r\n"^
"	endif;\r\n"^
"      endif;\r\n"^
"      if /* K>=T[J] */ brandom then\r\n"^
"	continue=0;\r\n"^
"      else\r\n"^
"	/* T[I]=T[J]; */\r\n"^
"	I = J;\r\n"^
"	J = 2*J;\r\n"^
"      endif;\r\n"^
"    done;\r\n"^
"    /* T[I] = K; */\r\n"^
"    if (L>=2) then\r\n"^
"      L = L-1; /* K = T[L]; */\r\n"^
"    else\r\n"^
"      /* K = T[R]; T[R]=T[1]; */\r\n"^
"      R = R-1;\r\n"^
"    endif;\r\n"^
"    /* T[1] = K; */\r\n"^
"  done;\r\n"^
"end\r\n"^
"\r\n"^
"var N:int,res:int;\r\n"^
"begin\r\n"^
"  res = heapsort(N);\r\n"^
"end\r\n"

let symmetricalstairs = 
"var x:int,y:int;\r\n"^
"\r\n"^
"begin\r\n"^
"  x = 0;\r\n"^
"  y = 0;\r\n"^
"  while (x<=99) do\r\n"^
"    if x<=49 then\r\n"^
"      x = x+1;\r\n"^
"      y = y+1;\r\n"^
"    else\r\n"^
"      x = x+1;\r\n"^
"      y = y-1;\r\n"^
"    endif;\r\n"^
"  done;\r\n"^
"  if y<0 then fail; endif;\r\n"^
"end\r\n"


(* ********************************************************************** *)
(* analyze *)
(* ********************************************************************** *)
let myescape_html s =
  let buf = Buffer.create 16 in
  let i = ref 0 in
  while !i < String.length s do
    if s.[!i]='<' then begin
      if (String.sub s (!i+1) 11)="span style=" then begin
	let index_end = String.index_from s (!i + 12) '>' in
	Buffer.add_string buf 
	  (String.sub s !i (index_end + 1 - !i));
	i := index_end + 1;
      end
      else if (String.sub s (!i+1) 6)="/span>" then begin
	Buffer.add_string buf "</span>";
	i := !i + 7;
      end
      else begin
	Buffer.add_string buf "&lt;";
	incr i;
      end
    end
    else begin
      if s.[!i]='>' then Buffer.add_string buf "&gt;" 
      else if s.[!i]='&' then Buffer.add_string buf "&amp;"
      else if s.[!i]='"' then Buffer.add_string buf "&quot;" 
      else Buffer.add_char buf s.[!i]
      ;
      incr i;
    end;
  done;
  Buffer.contents buf

let analyze (progtext:string) =
  let d = Date.get_date ()
  and e = Date.get_date () 
  in
  Date.add_minutes e 15;
  Html.h1 "Analysis Result";

  Html.p ("Run "^Link.url^" ?");

  let buffer = Buffer.create (String.length progtext) in
  let (output:Format.formatter) = Format.formatter_of_buffer buffer in
  begin try
    Option.displaytags := Option.htmltags;
    (* Parsing the program *)
    let lexbuf = Lexing.from_string progtext in
    let prog = Frontend.parse_lexbuf output lexbuf in
    (* Computing solution *)
    Frontend.analyze_display output prog;
    ()
  with
  | Exit -> ()
  | Failure s ->
      Html.h2 "Source";
      Html.pre progtext;

      Html.p (Html.escape_html s)
  end;

  Html.h2 "Result";
  print_string "<pre>\r\n";
  print_string (myescape_html (Buffer.contents buffer));
  Buffer.clear buffer;
  print_string "</pre>\r\n";
  Html.h2 "Source";
  Html.pre progtext;

  Html.p ("Run "^Link.url^" ?");
  ()

(* ********************************************************************** *)
(* frontpage *)
(* ********************************************************************** *)

let frontpage () =
  Html.h1 "The Interproc Analyzer";
  Html.p
    (Printf.sprintf "\
This is a web interface to the %s analyzer connected \
to the %s and the %s, whose goal is to demonstrate the features \
of the APRON library and, to a less extent, of the Analyzer fixpoint engine, \
in the static analysis field."
      Link.interproc
      Link.apron
      Link.fixpoint
    );
  Html.form_begin ~meth:Multipart "interprocweb.cgi";

  Html.h2 "Arguments";
  Html.p ("\
Please type a program, upload a file from your hard-drive, \
or choose one the provided examples:"
  );
  Html.form_file ~maxlength:32768 "file";
  Html.br ();
  Html.form_menu "example"
    [
      Option (None,"none",         "user-supplied",  true);
      Option (None,"ackerman",     "Ackerman",     false);
      Option (None,"maccarthy91",  "Mac Carthy 91",      false);
      Option (None,"heapsort",     "Heap Sort", false);
      Option (None,"symmetricalstairs", "Symmetrical Stairs", false);
    ];
  Html.br ();
  Html.form_textarea ~default:"/* type your program here ! */" "text" 15 60;
  Html.br ();
  Html.form_menu "abstraction"
    [
      Option (None, "none", "Choose an Abstract Domain:", false);
      Option (None, "box", "box", false);
      Option (None, "octagon", "octagon", false);
      Option (None, "polka", "convex polyhedra (polka)", true);
      Option (None, "ppl", "convex polyhedra (PPL)", false);
      Option (None, "polkastrict", "strict convex polyhedra (polka)", false);
      Option (None, "pplstrict", "strict convex polyhedra (PPL)", false);
      Option (None, "pplgrid", "linear congruences (PPL)", false);
      Option (None, "polkagrid", "convex polyhedra + linear congruences", false);
    ];
  Html.form_text
    ~size:6
    ~maxlength:6
    ~default:"f"
    "analysis"
  ;
  print_string " Analysis type"; 

  print_string "<br>Iterations/Widening options:<br>";
  Html.form_checkbox
    ~checked:false
    "guided"
  ;
  print_string "guided iterations<br>";
  Html.form_checkbox
    ~checked:false
    "widening_first"
  ;
  print_string "widening first ";
  Html.form_text
    ~size:2
    ~maxlength:2
    ~default:"1"
    "widening_start"
  ;
  print_string " widening delay ";
  Html.form_text
    ~size:2
    ~maxlength:2
    ~default:"1"
    "widening_frequency"
  ;
  print_string " widening frequency<br>";
  Html.form_text
    ~size:2
    ~maxlength:2
    ~default:"2"
    "descending"
  ;
  print_string "descending steps<br>";

  print_string "\
<p>\n\
Hit the OK button to proceed: ";
  Html.form_submit ~label:"OK !" ();
  Html.form_reset ~label:"Reset" ();


  Html.form_end ();
  
  Html.h2 Link.simple_syntax;
  Html.p (Print.sprintf "Here are some program examples: %s" Link.program_examples);

  Html.h2 "Results";
  Html.p ~style:"note" "\
In order not to flood our web-server, analysis computation time is \
limited to 1min in this demonstration. Also note that result files are \
temporary files stored on our server that have a very short life-time."
  ;
  Html.p "The analysis computes an invariant at each program point.";
  Html.h2 "Informations";
  Html.p (Print.sprintf "\
The %s is freely available. It is written in C, with a OCaml binding. \
The %s analyzer and the %s are freely available, \
and are written in OCaml."
    Link.interproc
    Link.apron
    Link.fixpoint);
  Html.p
    (Print.sprintf "\
This CGI-WEB interface is written in OCaml using the %s, \
freely available"
      Link.ocamlhtml);
  ()


(* ********************************************************************** *)
(* main *)
(* ********************************************************************** *)

let main () =
  try
    let args = Cgi.get_cgi_args () in
(*
    Format.bprintf Format.stdbuf
      "%a"
      (Print.list
	(fun fmt (str1,ostr2) ->
	  Format.fprintf fmt "(%s,%s)"
	  str1
	  (begin match ostr2 with
	  | None -> "None"
	  | Some s -> "Some "^s
	  end)
	))
      args
    ;
    print_string "<pre>\r\n";
    print_string (myescape_html (Format.flush_str_formatter ()));
    print_string "</pre>\r\n";
*)
    let (text,args) = match args with
      | ("file",Some "")::
	  ("filecontent",Some "")::
	  ("example",Some "none")::
	  ("text",Some text)::
	  args 
	->
	  (text,args)
      | ("file",_)::
	  ("filecontent",Some text)::
	  ("example",Some "none")::
	  ("text",_)::
	  args
	->
	  (text,args)

      | ("file",_)::
	  ("filecontent",_)::
	  ("example",Some e)::
	  ("text",_ )::
	  args
	->
	  let text = match e with
	    | "ackerman"   -> ackerman
	    | "maccarthy91"     -> maccarthy91
	    | "heapsort" -> heapsort
	    | "symmetricalstairs" -> symmetricalstairs
	    | _ -> "begin\n  end\n"
	  in
	  (text,args)
      | _ -> raise Exit
    in
 
    Solving.iteration_guided := false;
    Solving.widening_first := false;

    List.iter
      (begin function
	| ("abstraction",Some name) ->
	    Option.domain := List.assoc name Option.assocnamedomain;
	| ("analysis", Some text) ->
	    Option.analysis := [];
	    String.iter
	      (begin fun chr ->
		match chr with
		| 'f' ->
		    Option.analysis := Option.Forward :: !Option.analysis
	      | 'b' ->
		  Option.analysis := Option.Backward :: !Option.analysis
	      | _ ->
		  raise (Arg.Bad ("Wrong argument `"^text^"'; option `-analysis' expects only 'f' or 'b' characters in its argument string"))
	    end)
	    text;
	    Option.analysis := List.rev !Option.analysis;
	    if !Option.analysis=[] then 
	      Option.analysis := [Option.Forward]
	    ;
 	| ("guided",Some "on") ->
	    Solving.iteration_guided := true
	| ("widening_first",Some "on") ->
	    Solving.widening_first := true
	| ("widening_start",Some text) ->
	    Solving.widening_start := int_of_string text;
	| ("widening_frequency",Some text) ->
	    Solving.widening_freq := int_of_string text;
	| ("descending",Some text) ->
	    Solving.widening_descend := int_of_string text;
	| _ -> ()
      end)
      args
    ;
    analyze text
  with Exit ->
    frontpage ()

let _ =
  Cgi.set_timeout 15;

  Sscookie.clean_cookies "interprochtml";

  Http.http_header ();
  
  Html.html_begin
    ~lang:"en"
    ~author:"Antoine Min&eacute and Bertrand Jeannet"
    ~desc:"\
CGI interface to the Interproc static analyzer, \
illustrating the use of the APRON Abstract Domain Library"
    "Interproc Analyzer"
  ;
  main ();
  html_end
    ~author:"Antoine Min&eacute and Bertrand Jeannet"
    ~email:"bjeannet@NOSPAM inrialpes.fr"
    ()
  ;
  ()
