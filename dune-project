(lang dune 2.7)
(name ppx_macro)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(maintainers "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")

(package
 (name ppx_macro)
 (synopsis "A PPX rewriter to define and use macro values")
 (description "This package allows to create macros with `let%macro foo = bar` and use them with `[%use foo]`. The value of `[%use foo]` will depend on the local binding for `bar`.")
 (depends
  (alcotest (>= 1.6.0))
  (dune (>= 2.7))
  (ppxlib (>= 0.27.0))))
