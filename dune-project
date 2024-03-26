(lang dune 3.13)

(name devoir_de_programmation)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license GPL)

(documentation https://url/to/documentation)

(package
 (name devoir_de_programmation)
 (synopsis "Devoir de programmation")
 (description "Devoir de programmation du cours L2 Programmation fonctionnelle")
 (depends ocaml dune ounit2 ocamlformat utop)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
