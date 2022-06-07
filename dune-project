(lang dune 3.0)

(using ctypes 0.1)

(name gdal)

(generate_opam_files true)

(source
 (github geocaml/ocaml-gdal))

(authors "Patrick Ferris")

(maintainers "Patrick Ferris")

(license LICENSE)

(documentation https://ocaml.org/p/gdal)

(package
 (name gdal)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends geo)
 (tags
  (geospatial geocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
