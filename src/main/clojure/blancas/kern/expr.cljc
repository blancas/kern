;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Support for the evaluation of expressions."
      :author "Armando Blancas"}
  blancas.kern.expr
  (:require [blancas.kern.core :as k :refer [<|> return >>]]
            [blancas.kern.i18n :as i18n]
            [blancas.kern.lexer.c-style :as c-style])
  #?(:cljs (:require-macros [blancas.kern.core :as k])))


;; +-------------------------------------------------------------+
;; |                  Expression Chaining.                       |
;; +-------------------------------------------------------------+


(defn chainl1
  "Parses p; as long as there is a binary operator op, reads the op and
   another instance of p, then applies the operator on both values.
   The operator associates to the left."
  [p op]
  (letfn [(rest [a] (<|> (k/bind [f op b p] (rest (f a b)))
                         (return a)))]
    (k/bind [a p] (rest a))))


(defn chainl
  "Parses a value or a sequence or values separated by a binary operator.
   If there's no initial value, defaults to a. Associates to the left."
  [p op a] (<|> (chainl1 p op) (return a)))


(defn chainr1
  "Parses p; as long as there is a binary operator op, reads the op and
   calls itself to compute the rest of the expression, then it applies
   the operator on both values. The operator associates to the right."
  [p op]
  (k/bind [a p]
    (<|> (k/bind [f op b (chainr1 p op)] (return (f a b)))
	 (return a))))


(defn chainr
  "Parses a value or a sequence or values with infix binary operators.
   If there's no initial value, defaults to a. Associates to the right."
  [p op a] (<|> (chainr1 p op) (return a)))


(defn chainl1*
  "Parses p; as long as there is a binary operator op, reads the op and
   another p, then makes an AST node with the operator on both values.
   The operator associates to the left."
  [tok p op]
  (letfn [(rest [a] (<|> (k/bind [f op b p]
			   (rest {:token tok :op f :left a :right b}))
                         (return a)))]
    (k/bind [a p] (rest a))))


(defn chainl*
  "Parses a value or a sequence or values separated by a binary operator.
   If there's no initial value, defaults to a. Associates to the left.
   The resulting value is an AST node."
  [tok p op a] (<|> (chainl1* tok p op) (return a)))


(defn chainr1*
  "Parses p; as long as there is a binary operator op, reads the op and
   calls itself to make the rest of the expression AST, then it makes
   an AST node with the operator on both values. The operator associates
   to the right."
  [tok p op]
  (k/bind [a p]
	(<|> (k/bind [f op b (chainr1* tok p op)]
	       (return {:token tok :op f :left a :right b}))
	 (return a))))


(defn chainr*
  "Parses a value or a sequence or values with infix binary operators.
   If there's no initial value, defaults to a. Associates to the right.
   The operator associates to the right."
  [tok p op a] (<|> (chainr1* tok p op) (return a)))


(defn prefix1
  "Parses zero or more operators op before an operand p. It applies
   the parsed functions to the operand in reverse order of parsing."
  [p op]
  (<|> (k/bind [f op a (prefix1 p op)] (return (f a)))
       (k/bind [a p] (return a))))


(defn prefix
  "Like prefix1, but both the operator and operand are optional.
   If no operand is given, it returns the default value a."
  [p op a] (<|> (prefix1 p op) (return a)))


(defn prefix1*
  "Parses zero or more operators op before an operand p. It builds an
   AST node for each parsed function, where the operand is a node
   for a value or a further application of a prefix operator."
  [tok p op]
  (<|> (k/bind [f op a (prefix1* tok p op)] (return {:token tok :op f :right a}))
       (k/bind [a p] (return a))))


(defn prefix*
  "Like prefix1*, but both the operator and operand are optional.
   If no operand is given, it returns the default value a."
  [tok p op a] (<|> (prefix1* tok p op) (return a)))


(defn postfix1
  "Parses an operand p followed by zero or more operators. It applies the
   parsed functions to the operand or the result of a previous application."
  [p op]
  (letfn [(rest [a] (<|> (k/bind [f op] (rest (f a)))
                         (return a)))]
    (k/bind [a p] (rest a))))


(defn postfix
  "Like postfix1, but both the operator and operand are optional.
   If no operand is given, it returns the default value a."
  [p op a] (<|> (postfix1 p op) (return a)))


(defn postfix1*
  "Parses an operand p followed by zero or more operators op. It builds
   an AST node for each parsed function, where the operand is a node
   for a value or a previous  application of a postfix operator."
  [tok p op]
  (letfn [(rest [a] (<|> (k/bind [f op] (rest {:token tok :op f :left a}))
                         (return a)))]
    (k/bind [a p] (rest a))))


(defn postfix*
  "Like postfix1*, but both the operator and operand are optional.
   If no operand is given, it returns the default value a."
  [tok p op a] (<|> (postfix1* tok p op) (return a)))


;; +-------------------------------------------------------------+
;; |                      Operator parsers.                      |
;; +-------------------------------------------------------------+


(def pow-op
  "Parses the POW operator."
  (>> (c-style/sym \^) (return #(Math/pow %1 %2))))


(def uni-op
  "Unary prefix operator: logical not or numeric minus."
  (k/bind [op (c-style/one-of "!-")]
    (return ({\! not \- -} op))))


(def mul-op
  "Multiplicative operator: multiplication, division, or modulo."
  (k/bind [op (c-style/one-of "*/%")]
    (return ({\* * \/ quot \% mod} op))))


(def add-op
  "Additive operator: addition or subtraction."
  (k/bind [op (c-style/one-of "+-")]
    (return ({\+ + \- -} op))))


(def rel-op
  "Parses one of the relational operators."
  (k/bind [op (c-style/token "==" "!=" ">=" "<=" ">" "<")]
    (return ({"==" = "!=" not= ">=" >= "<=" <= ">" > "<" <} op))))


(def and-op
  "Parses the logical AND operator."
  (>> (c-style/token "&&") (return #(and %1 %2))))


(def or-op
  "Parses the logical OR operator."
  (>> (c-style/token "||") (return #(or %1 %2))))
