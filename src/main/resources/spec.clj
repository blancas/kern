(ns ^{:author "Armando Blancas"
      :doc "Parses record declarations in table format.
SPEC <name>
+--------------+--------------+-----+--------------+
| field1 type1 | field2 type2 | ... | fieldn typeN |
+--------------+--------------+-----+--------------+"}
  spec
  (:use [blancas.kern.core]
        [blancas.kern.lexer.c-style]))

;; To try:
(comment
(load "spec")
(ns spec)
(run pgm (slurp "src/main/resources/spec.dsl"))
)

(def spec
  "Parses the spec name: skip over 'SPEC' and parse the identifier."
  (>> (word "SPEC") identifier))

(def line
  "Parses a separating line: read a plus sign, followed by one
   or more sequences of dashes ending with a plus sign."
  (lexeme (skip (sym* \+) 
                (many1 (<*> (many1 (sym* \-)) (sym* \+))))))

(def ftype
  "Reads one of the listed tokens."
  (token "int8" "int16" "int32" "float32" "float64" "string"))

(def fields
  "To read the fields: skip over the first bar, then read one
   or more id-type pairs ending with a bar."
  (>> (sym \|) (many1 (<< (<*> identifier ftype) (sym \|)))))

(def record
  "Parses the rule:  record := spec line fields line"
  (bind [sp spec _ line fs fields _ line]
    (return {:spec sp :fields fs})))

(def stmt
  "Parses the rule:  stmt := ('read' | 'write') ID 'as' ID"
  (bind [cmd (word "read" "write") buf identifier _ (word "as") s identifier]
	(return {:token (keyword cmd) :buffer buf :spec s})))

(def pgm
  "Parses the rule:  pgm := record stmt*
   trim removes any leading whitespaces at the start of the input."
  (<*> trim record (many stmt)))
