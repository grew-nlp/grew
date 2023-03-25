(lang dune 3.5)

(name grew)

(version 1.12.0)

(generate_opam_files true)

(source
 (github grew-nlp/grew))

(authors "Bruno Guillaume")

(maintainers "Bruno.Guillaume@inria.fr")

(license CECILL-2.1)

(documentation https://grew.fr)

(package
 (name grew)
 (synopsis " Grew system")
 (description "CLI for Grew (Graph Rewriting for NLP)")
 (depends 
  ocaml 
  dune 
  (containers ( >= "3.6" ))
  amr 
  conll 
  (grewlib ( >= "1.12.0" ))
 )
 (tags
  (nlp "graph rewriting")
 )
)
