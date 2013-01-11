;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-haskell
  (:use [blancas.kern.core]
	[blancas.kern.lexer.haskell-style]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))

;; Private functions from kern.core

(def get-msg-str (ns-resolve 'blancas.kern.core 'get-msg-str)) 


;; +-------------------------------------------------------------+
;; |                  Haskell-style lexers.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> trim eof) "  \t\t\n")]
    (fact "trim - blank, tab, eol, then eof"
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0005
  (let [s1 (parse (>> trim (many digit)) "123")]
    (fact "trim - no whitespace, it's optional"
	  (:value s1)  =>  [\1 \2 \3]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0010
  (let [s1 (parse (>> trim (many1 letter)) "  {- comment -} \t\n\t\t ABC")]
    (fact "trim - whitespace before letters"
	  (:value s1)  =>  [\A \B \C]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0015
  (let [s1 (parse (>> (lexeme (sym* \space)) eof) "  \t\t\n")]
    (fact "lexeme - a blank, then tab, eol; then eof"
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0020
  (let [s1 (parse (lexeme (many digit)) "123")]
    (fact "lexeme - no whitespace, it's optional"
	  (:value s1)  =>  [\1 \2 \3]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0025
  (let [s1 (parse (lexeme (many1 letter)) "ABC {- that's it -} \t\n\t\t")]
    (fact "lexeme - whitespace and comments after letters"
	  (:value s1)  =>  [\A \B \C]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0030
  (let [s1 (parse (lexeme (many1 letter)) "foo -- and the rest is history\nbar")]
    (fact "trim - single-line comment"
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\b \a \r]
	  (:empty s1)  =>  false)))


(deftest test-0035
  (let [in "foo -- variable\n-- that's all\n-- for now\nbar"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple single-line comments"
	  (:input s1)  =>  [\b \a \r]
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0040
  (let [in "foo {- var foo\n  that's all\n for now -}\nbar"
        s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple multi-line comment"
	  (:input s1)  =>  [\b \a \r]
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0045
  (let [in "foo{-----this is a comment------}"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple multi-line comment"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0050
  (let [in "foo{-----this is {---- inner comment ----} an outer comment------}"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips nested comments"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0055
  (let [in "foo{---outer {---- inner {---- most inner ----} less inner ----} outer ----}"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips nested comments"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0060
  (let [in "foo{-----this is a comment------"
	s1 (parse (lexeme (many1 letter)) in)
    	em (get-msg-str (:error s1))]
    (fact "lexeme - fails looking for end of comment"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected end of input\nexpecting end of comment")))


(deftest test-0065
  (let [in "foo{-----this is a {- NESTED comment------}"
	s1 (parse (lexeme (many1 letter)) in)
    	em (get-msg-str (:error s1))]
    (fact "lexeme - fails looking for end of a nested comment"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected end of input\nexpecting end of comment")))


(deftest test-0070
  (let [s1 (parse char-lit "'z'")]
    (fact "char-lit"
	  (:value s1)  =>  \z
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0075
  (let [s1 (parse char-lit "'\\b'")]
    (fact "char-lit"
	  (:value s1)  =>  \backspace
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0080
  (let [s1 (parse char-lit "'\\t'")]
    (fact "char-lit"
	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0085
  (let [s1 (parse char-lit "'\\n'")]
    (fact "char-lit"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0090
  (let [s1 (parse char-lit "'\\f'")]
    (fact "char-lit"
	  (:value s1)  =>  \formfeed
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0095
  (let [s1 (parse char-lit "'\\r'")]
    (fact "char-lit"
	  (:value s1)  =>  \return
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0100
  (let [s1 (parse char-lit "'\\''")]
    (fact "char-lit"
	  (:value s1)  =>  \'
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0105
  (let [s1 (parse char-lit "'\\\"'")]
    (fact "char-lit"
	  (:value s1)  =>  \"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0110
  (let [s1 (parse char-lit "'\\\\'")]
    (fact "char-lit"
	  (:value s1)  =>  \\
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0115
  (let [s1 (parse char-lit "'\\/'")]
    (fact "char-lit"
	  (:value s1)  =>  \/
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0120
  (let [s1 (parse char-lit "'\\x0041'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0125
  (let [s1 (parse char-lit "'\\o101'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0130
  (let [s1 (parse char-lit "'\\90'")]
    (fact "char-lit"
	  (:value s1)  =>  \Z
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))
