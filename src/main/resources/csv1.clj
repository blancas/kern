(ns ^{:doc "Sample CSV parser for files with a header line."
      :author "Armando Blancas"}
  csv1
  (:use [blancas.kern.core]
        [blancas.kern.lexer.basic]))

;; To try:
(comment
(load "csv1")
(ns csv1)
(run csv (slurp "src/main/resources/data1.csv"))
)

(def headers
  "Reads a list of field names."
  (comma-sep identifier))

(def record
  "A record has one or more string, floating-point, or field."
  (comma-sep1 (<|> string-lit float-lit (field ",\n"))))

(def csv
  "Parses a header, followed by at least one record."
  (bind [hs headers rs (many1 record)]
    (return (list hs rs))))
