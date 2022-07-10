(lang dune 3.3)

(name bite)

(generate_opam_files true)

(source
 (github waterlens/bite))

(authors "Waterlens")

(maintainers "Waterlens")

(license MIT)

(documentation https://github.com/waterlens/bite)

(package
 (name bite)
 (synopsis "A language for experimental purpose")
 (depends ocaml dune)
 (tags
  (lang)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
