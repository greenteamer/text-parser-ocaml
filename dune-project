(lang dune 3.16)

(name parser)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name parser)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune 
   (batteries (= 3.8.0))
   (ocamlformat (= 0.26.2))
   (batteries (= 3.8.0))
   (yojson (= 2.2.2)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
