(lang dune 2.7)
(name ppx_inline_module)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(maintainers "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(homepage "https://github.com/bensmrs/ppx_inline_module")
(source (github "bensmrs/ppx_inline_module"))
(bug_reports "https://github.com/bensmrs/ppx_inline_module/issues")

(package
 (name ppx_inline_module)
 (synopsis "A PPX rewriter to allow for one-line dotted expressions on packed modules")
 (description "This package enables developers to write one-line dotted expressions on packed modules by writing `[%imod m.A.B.c]` instead of `let module M = (val m) in M.A.B.c`")
 (depends
  (alcotest (>= 1.6.0))
  (ppxlib (>= 0.27.0))))
