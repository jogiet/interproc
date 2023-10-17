(lang dune 3.7)

(name Interproc)
(version 28642a9-dirty)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Bertrand Jeannet")

(maintainers "Maintainer Name")

(license LGPL)

(documentation https://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/interproc/index.html)

(package
 (name Interproc)
 (synopsis "Interprocedural analyzer")
 (description "Interproc is an interprocedural analyzer for a small imperative language with (recursive) procedure calls. It infers invariants on the numerical variables of analyzed program.")
 (depends ocaml dune apron fixpoint camllib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
