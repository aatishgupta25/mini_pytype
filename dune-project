(lang dune 3.14)

; ---- project metadata ----------------------------------------------------
(name mini-pytype)
(version 0.1)

(generate_opam_files true)

(package
 (name mini-pytype)
 (synopsis "Static type checker for a Python-3.10 subset")
 ;; runtime deps for the scaffold
 (depends
   yojson
   cmdliner
   alcotest))
