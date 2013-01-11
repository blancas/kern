(ns ^{:doc "Sample CSV parser for files with a header line and text fields."
      :author "Armando Blancas"}
  csv2
  (:use [blancas.kern.core]
        [blancas.kern.lexer.basic]))

;; To try:
(comment
(load "csv2")
(ns csv2)
(run csv (slurp "src/main/resources/data2.csv"))
)

(def headers
  "Reads a list of field names."
  (comma-sep identifier))

(def record
  "A record is one of more fields separated by commas."
  (comma-sep1 (field ",\n")))

(def csv
  "Parses a header, followed by at least one record."
  (bind [hs headers rs (many1 record)]
    (return (list hs rs))))
