(lang dune 3.10)

(name ocontext)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name ocontext)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml (>= "5.0"))
   (dune (>= "3.8.2"))
   (alcotest :with-test)
   dream dream-livereload
   tyxml
   bos
   dotenv
   str
   petrol
   base fmt ppx_let ppx_enumerate ppxlib ppx_deriving ppx_deriving_yojson
   lwt caqti caqti-lwt caqti-dynload caqti-driver-sqlite3
   cohttp-lwt-unix)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
