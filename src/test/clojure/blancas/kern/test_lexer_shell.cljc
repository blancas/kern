;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-shell
  (:require [blancas.kern.core :as k :refer [parse >> <*>]]
            [blancas.kern.lexer.shell-style :as lex]
            [clojure.test :refer [deftest is testing]]))


;; +-------------------------------------------------------------+
;; |                    Shell-style lexers.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> lex/trim lex/new-line) "  \t\t\n")]
    (testing "trim - blank, tab, eol, then a new line (separately)"
      (is (= \newline (:value s1)))
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
  (let [s1 (parse (>> (k/skip lex/trim lex/new-line) (k/many1 k/letter)) " \t\t\nABC")]
    (testing "trim - some whitespace before letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0015
  (let [s1 (parse (<*> (lex/lexeme (k/sym* \space)) lex/new-line k/eof) "  \t\t\n")]
    (testing "lexeme - a blank, then tab, eol; then eof"
      (is (= [\space \newline nil] (:value s1)))
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
  (let [s1 (parse (lex/lexeme (k/many1 k/letter)) "ABC \t\t\t")]
    (testing "lexeme - some whitespace after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0030
  (let [s1 (parse lex/new-line "\nfoo")]
    (testing "new-line - parses a new line and stops"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0035
  (let [s1 (parse lex/new-line "\n\t\t   foo")]
    (testing "new-line - skip a new line and any other whitespace that follows"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0040
  (let [s1 (parse lex/new-line "\r\nfoo")]
    (testing "new-line - parses a Windows new-line and stops"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0045
  (let [s1 (parse lex/new-line "foo")
        em (k/get-msg-str (:error s1))]
    (testing "new-line - fails when there's no new-line"
      (is (= [\f \o \o] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj  "unexpected \\f\nexpecting new line"
                :cljs "unexpected \"f\"\nexpecting new line")
             em)))))


(deftest test-0050
  (let [s1 (parse (lex/lexeme (k/many k/digit)) "123 # and the rest is history...")]
    (testing "lexeme - skip over a line comment"
      (is (= [\1 \2 \3] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0055
  (let [s1 (parse lex/identifier "init.phase-123_last=")]
    (testing "lexeme - identifier with - and ."
      (is (= "init.phase-123_last" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\=] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0060
  (let [s1 (parse (k/many1 lex/identifier) "abc def ghi \\\nxyz")]
    (testing "lexeme - line continuation; no new line to skip"
      (is (= ["abc" "def" "ghi" "xyz"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))
