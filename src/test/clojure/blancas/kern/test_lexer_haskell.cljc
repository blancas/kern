;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-haskell
  (:require [blancas.kern.core :as k :refer [parse >>]]
            [blancas.kern.lexer.haskell-style :as lex]
            [clojure.test :refer [deftest is testing]]))

;; +-------------------------------------------------------------+
;; |                  Haskell-style lexers.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> lex/trim k/eof) "  \t\t\n")]
    (testing "trim - blank, tab, eol, then eof"
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0005
  (let [s1 (parse (>> lex/trim (k/many k/digit)) "123")]
    (testing "trim - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0010
  (let [s1 (parse (>> lex/trim (k/many1 k/letter)) "  {- comment -} \t\n\t\t ABC")]
    (testing "trim - whitespace before letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0015
  (let [s1 (parse (>> (lex/lexeme (k/sym* \space)) k/eof) "  \t\t\n")]
    (testing "lexeme - a blank, then tab, eol; then eof"
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0020
  (let [s1 (parse (lex/lexeme (k/many k/digit)) "123")]
    (testing "lexeme - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0025
  (let [s1 (parse (lex/lexeme (k/many1 k/letter)) "ABC {- that's it -} \t\n\t\t")]
    (testing "lexeme - whitespace and comments after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0030
  (let [s1 (parse (lex/lexeme (k/many1 k/letter)) "foo -- and the rest is history\nbar")]
    (testing "trim - single-line comment"
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\b \a \r] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0035
  (let [in "foo -- variable\n-- that's all\n-- for now\nbar"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple single-line comments"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0040
  (let [in "foo {- var foo\n  that's all\n for now -}\nbar"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0045
  (let [in "foo{-----this is a comment------}"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (empty? (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0050
  (let [in "foo{-----this is {---- inner comment ----} an outer comment------}"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips nested comments"
      (is (empty? (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0055
  (let [in "foo{---outer {---- inner {---- most inner ----} less inner ----} outer ----}"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips nested comments"
      (is (empty? (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0060
  (let [in "foo{-----this is a comment------"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)
        em (k/get-msg-str (:error s1))]
    (testing "lexeme - fails looking for end of comment"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "unexpected end of input\nexpecting end of comment" em)))))


(deftest test-0065
  (let [in "foo{-----this is a {- NESTED comment------}"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)
        em (k/get-msg-str (:error s1))]
    (testing "lexeme - fails looking for end of a nested comment"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "unexpected end of input\nexpecting end of comment" em)))))


(deftest test-0070
  (let [s1 (parse lex/char-lit "'z'")]
    (testing "char-lit"
      (is (= \z (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0075
  (let [s1 (parse lex/char-lit "'\\b'")]
    (testing "char-lit"
      (is (= \backspace (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0080
  (let [s1 (parse lex/char-lit "'\\t'")]
    (testing "char-lit"
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0085
  (let [s1 (parse lex/char-lit "'\\n'")]
    (testing "char-lit"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0090
  (let [s1 (parse lex/char-lit "'\\f'")]
    (testing "char-lit"
      (is (= \formfeed (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0095
  (let [s1 (parse lex/char-lit "'\\r'")]
    (testing "char-lit"
      (is (= \return (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0100
  (let [s1 (parse lex/char-lit "'\\''")]
    (testing "char-lit"
      (is (= \' (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0105
  (let [s1 (parse lex/char-lit "'\\\"'")]
    (testing "char-lit"
      (is (= \" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0110
  (let [s1 (parse lex/char-lit "'\\\\'")]
    (testing "char-lit"
      (is (= \\ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0115
  (let [s1 (parse lex/char-lit "'\\/'")]
    (testing "char-lit"
      (is (= \/ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0120
  (let [s1 (parse lex/char-lit "'\\x0041'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0125
  (let [s1 (parse lex/char-lit "'\\o101'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0130
  (let [s1 (parse lex/char-lit "'\\90'")]
    (testing "char-lit"
      (is (= \Z (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))
