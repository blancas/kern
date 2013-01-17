(ns ^{:doc "A sample JSON parser."
      :author "Armando Blancas"}
  json
  (:use [blancas.kern.core]
        [blancas.kern.lexer.basic]))

;; To try:
(comment
(load "json")
(ns json)
(run jvalue (slurp "src/main/resources/tweet.json"))
)

(declare jvalue)

(def pair
  "Parses the rule:  pair := String ':' jvalue"
  (bind [f string-lit _ colon v (fwd jvalue)]
    (return [f v])))

(def array
  "Parses the rule:  array := '[' (jvalue (',' jvalue)*)* ']'"
  (brackets
    (bind [elements (comma-sep (fwd jvalue))]
      (return (vec elements)))))
  
(def object
  "Parses the rule:  object := '{' (pair (',' pair)*)* '}'"
  (braces
    (bind [members (comma-sep pair)]
      (return (apply hash-map (reduce concat [] members))))))

(def jvalue
  "Parses a JSON value."
  (<|> string-lit dec-lit float-lit bool-lit nil-lit array object))
