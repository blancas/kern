;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-shell
  (:use [blancas.kern.core]
	[blancas.kern.lexer.shell-style]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))

;; Private functions from kern.core

(def get-msg-str (ns-resolve 'blancas.kern.core 'get-msg-str)) 


;; +-------------------------------------------------------------+
;; |                    Shell-style lexers.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> trim new-line) "  \t\t\n")]
    (fact "trim - blank, tab, eol, then a new line (separately)"
	  (:value s1)  =>  \newline
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
  (let [s1 (parse (>> (skip trim new-line)  (many1 letter)) " \t\t\nABC")]
    (fact "trim - some whitespace before letters"
	  (:value s1)  =>  [\A \B \C]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0015
  (let [s1 (parse (<*> (lexeme (sym* \space)) new-line eof) "  \t\t\n")]
    (fact "lexeme - a blank, then tab, eol; then eof"
	  (:value s1)  =>  [\space \newline nil]
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
  (let [s1 (parse (lexeme (many1 letter)) "ABC \t\t\t")]
    (fact "lexeme - some whitespace after letters"
	  (:value s1)  =>  [\A \B \C]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0030
  (let [s1 (parse new-line "\nfoo")]
    (fact "new-line - parses a new line and stops"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\f \o \o]
	  (:empty s1)  =>  false)))


(deftest test-0035
  (let [s1 (parse new-line "\n\t\t   foo")]
    (fact "new-line - skip a new line and any other whitespace that follows"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\f \o \o]
	  (:empty s1)  =>  false)))


(deftest test-0040
  (let [s1 (parse new-line "\r\nfoo")]
    (fact "new-line - parses a Windows new-line and stops"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\f \o \o]
	  (:empty s1)  =>  false)))


(deftest test-0045
  (let [s1 (parse new-line "foo")
    	em (get-msg-str (:error s1))]
    (fact "new-line - fails when there's no new-line"
	  (:input s1)  =>  [\f \o \o]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\f\nexpecting new line")))


(deftest test-0050
  (let [s1 (parse (lexeme (many digit)) "123 # and the rest is history...")]
    (fact "lexeme - skip over a line comment"
	  (:value s1)  =>  [\1 \2 \3]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0055
  (let [s1 (parse identifier "init.phase-123_last=")]
    (fact "lexeme - identifier with - and ."
	  (:value s1)  =>  "init.phase-123_last"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\=]
	  (:empty s1)  =>  false)))


(deftest test-0060
  (let [s1 (parse (many1 identifier) "abc def ghi \\\nxyz")]
    (fact "lexeme - line continuation; no new line to skip"
	  (:value s1)  =>  ["abc" "def" "ghi" "xyz"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))
