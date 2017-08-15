;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Kern C-style Lexer library.

This namespace is a version of blancas.kern.lexer with the following settings:

comment-start        /*
comment-end          */
comment-line         //
nested-comments      No
identifier-start     Letter or _
identifier-letter    Alphanumeric or _ 
reserved-names       None
case-sensitive       Yes
line-continuation    Backslash
trim-newline         Yes

Literal values follow the rules of the C programming language."
      :author "Armando Blancas"}
  blancas.kern.lexer.c-style
  (:require [blancas.kern.core :as k]
            [blancas.kern.lexer :as lex]))

(def rec (lex/make-parsers lex/c-style-def))

(def trim       (:trim       rec))
(def lexeme     (:lexeme     rec))
(def sym        (:sym        rec))
(def new-line   (:new-line   rec))
(def one-of     (:one-of     rec))
(def none-of    (:none-of    rec))
(def token      (:token      rec))
(def word       (:word       rec))
(def identifier (:identifier rec))
(def field      (:field      rec))
(def char-lit   (:char-lit   rec))
(def string-lit (:string-lit rec))
(def dec-lit    (:dec-lit    rec))
(def oct-lit    (:oct-lit    rec))
(def hex-lit    (:hex-lit    rec))
(def float-lit  (:float-lit  rec))
(def bool-lit   (:bool-lit   rec))
(def nil-lit    (:nil-lit    rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def angles     (:angles     rec))
(def brackets   (:brackets   rec))
(def semi       (:semi       rec))
(def comma      (:comma      rec))
(def colon      (:colon      rec))
(def dot        (:dot        rec))
(def semi-sep   (:semi-sep   rec))
(def semi-sep1  (:semi-sep1  rec))
(def comma-sep  (:comma-sep  rec))
(def comma-sep1 (:comma-sep1 rec))
