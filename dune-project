(lang dune 3.9)

(name gocaml)

(generate_opam_files true)

(source
 (github CharlesAverill/gocaml))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license LICENSE)

(documentation https://github.com/CharlesAverill/gocaml)

(package
 (name gocaml)
 (synopsis "An OCaml Go Engine")
 (description "An OCaml Go Engine")
 (depends ocaml dune graphics))

