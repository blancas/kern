(ns ^{:doc "Parsing of a simple INI file."
      :author "Armando Blancas"}
  ini
  (:use [blancas.kern.core]
        [blancas.kern.lexer.c-style]))

;; To try:
(comment
(load "ini")
(ns ini)
(run ini (slurp "src/main/resources/sample.ini"))
)

(def pair
  "Parses this rule:  pair := ID = field ( , field )* and returns the
   name-value pair in a vector. Operator <?> allows an error message to
   state that it expects a property, as oposed to saying it expects a letter."
  (bind [f (<?> identifier "property") _ (sym \=) v (comma-sep1 (field ",\n"))]
    (return [f v])))

(def group
  "Parses this rule:  group := [ field ] pair+"
  (bind [hdr (brackets (field "]\n")) rec (many1 pair)]
    (return [hdr (apply hash-map (flatten rec))])))

(def ini
  "Parses:  init := group+"
  (many1 group))
