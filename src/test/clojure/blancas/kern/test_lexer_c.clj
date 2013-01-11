;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-c
  (:use [blancas.kern.core]
	[blancas.kern.lexer.c-style]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))

;; Private functions from kern.core

(def get-msg-str (ns-resolve 'blancas.kern.core 'get-msg-str)) 


;; +-------------------------------------------------------------+
;; |                    Java-style lexers.                       |
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
  (let [s1 (parse (>> trim (many1 letter)) "  /* comment */ \t\n\t\t ABC")]
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
  (let [s1 (parse (lexeme (many1 letter)) "ABC /* that's it */ \t\n\t\t")]
    (fact "lexeme - whitespace and comments after letters"
	  (:value s1)  =>  [\A \B \C]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0030
  (let [s1 (parse (lexeme (many1 letter)) "foo // and the rest is history\nbar")]
    (fact "trim - single-line comment"
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\b \a \r]
	  (:empty s1)  =>  false)))


(deftest test-0035
  (let [in "foo // variable\n// that's all\n// for now\nbar"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple single-line comments"
	  (:input s1)  =>  [\b \a \r]
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0040
  (let [in "foo /* var foo\n  that's all\n for now */\nbar"
        s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple multi-line comment"
	  (:input s1)  =>  [\b \a \r]
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0045
  (let [in "foo/********this is a comment**********/"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - skips over multiple multi-line comment"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0050
  (let [in "foo/****** this is a comment*****"
	s1 (parse (lexeme (many1 letter)) in)
    	em (get-msg-str (:error s1))]
    (fact "lexeme - fails looking for end of comment"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected end of input\nexpecting end of comment")))


(deftest test-0055
  (let [in "foo/******* this is a /* CAN I NEST? */ comment ********/"
	s1 (parse (lexeme (many1 letter)) in)]
    (fact "lexeme - Won't da nested comment; but this works and stops at 'comment'"
	  (:input s1)  =>  (seq "comment ********/")
	  (:value s1)  =>  [\f \o \o]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0060
  (let [s1 (parse char-lit "'z'")]
    (fact "char-lit"
	  (:value s1)  =>  \z
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0065
  (let [s1 (parse char-lit "'\\b'")]
    (fact "char-lit"
	  (:value s1)  =>  \backspace
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0070
  (let [s1 (parse char-lit "'\\t'")]
    (fact "char-lit"
	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0075
  (let [s1 (parse char-lit "'\\n'")]
    (fact "char-lit"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0080
  (let [s1 (parse char-lit "'\\f'")]
    (fact "char-lit"
	  (:value s1)  =>  \formfeed
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0085
  (let [s1 (parse char-lit "'\\r'")]
    (fact "char-lit"
	  (:value s1)  =>  \return
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0090
  (let [s1 (parse char-lit "'\\''")]
    (fact "char-lit"
	  (:value s1)  =>  \'
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0095
  (let [s1 (parse char-lit "'\\\"'")]
    (fact "char-lit"
	  (:value s1)  =>  \"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0100
  (let [s1 (parse char-lit "'\\\\'")]
    (fact "char-lit"
	  (:value s1)  =>  \\
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0105
  (let [s1 (parse char-lit "'\\/'")]
    (fact "char-lit"
	  (:value s1)  =>  \/
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0110
  (let [s1 (parse char-lit "'\\u0041'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0115
  (let [s1 (parse char-lit "'\\101'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0120
  (let [in "'a "
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (fact "char-lit - fails without the closing quote"
	  (:input s1)  =>  [\space]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\space\nexpecting end of character literal")))


(deftest test-0125
  (let [in "'\\u2'"
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (fact "char-lit - fails: incomplete unicode number"
	  (:input s1)  =>  [\']
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\'\nexpecting hexadecimal digit")))


(deftest test-0130
  (let [in "'\\00*"
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (fact "char-lit - fails: octal numbers are assumed; that's acually char \0"
	  (:input s1)  =>  [\0 \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\0\nexpecting end of character literal")))


(deftest test-0135
  (let [s1 (parse char-lit "'\\?'")]
    (fact "char-lit"
	  (:value s1)  =>  \?
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0140
  (let [s1 (parse char-lit "'\\a'")]
    (fact "char-lit"
	  (:value s1)  =>  (char 7)
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0145
  (let [s1 (parse char-lit "'\\v'")]
    (fact "char-lit"
	  (:value s1)  =>  (char 11)
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0150
  (let [s1 (parse char-lit "'\\0'")]
    (fact "char-lit"
	  (:value s1)  =>  (char 0)
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0155
  (let [s1 (parse char-lit "'\\x41'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0160
  (let [s1 (parse char-lit "'\\U00000041'")]
    (fact "char-lit"
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0165
  (let [in "\"\\bnow\\tis\\nthe\\ftime\\r\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a simple string literal"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "\bnow\tis\nthe\ftime\r"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0170
  (let [in "\"null-terminated\\0\";"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a null-terminated string"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  "null-terminated\u0000"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0175
  (let [in "\"\\\\null-terminated\\?\";"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a backslash and a question mark"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  "\\null-terminated?"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0180
  (let [in "\"\\tnow is \\u0074\\u0068\\u0065 time\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses unicode characters"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "\tnow is the time"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0185
  (let [in "\"now is \\164\\150\\145 time\" /* the */|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses octal characters"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "now is the time"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0190
  (let [in "\"now is \\x74\\x68\\x65 time\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses hex characters"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "now is the time"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))
