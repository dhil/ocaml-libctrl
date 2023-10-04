(lang dune 3.10)

(name libctrl)

(generate_opam_files true)

(source
 (github dhil/ocaml-libctrl))

(authors "Daniel Hillerström")

(maintainers "Daniel Hillerström <daniel.hillerstrom@ed.ac.uk>")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name libctrl)
 (synopsis "Continuations, continuations, and more continuations")
 (description "A playground library for programming with first-class control")
 (depends
  (ocaml (>= : 5.0.0))
   dune
  (ounit2 :with-test)
  (qcheck :with-test))
 (tags
  ("first-class control operators" "effect handlers" "shift/reset" "control/prompt" "fcontrol/run" "callcc" "C" "delimited continuations" "undelimited continuations")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
