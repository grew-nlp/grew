(lang dune 3.5)

(name grew)

(version 1.16.1)

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
  dune-build-info
  (containers ( >= "3.6" ))
  amr 
  (conll ( >= "1.19.1" ))
  (grewlib ( >= "1.16.3" ))
 )
 (tags
  (nlp "graph rewriting")
 )
)
