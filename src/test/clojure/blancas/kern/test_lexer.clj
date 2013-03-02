;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer
  (:use [blancas.kern.core]
	[blancas.kern.lexer.basic]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)])
  (:require [blancas.kern.lexer :as lex]))

;; Private functions from kern.core

(def get-msg-str (ns-resolve 'blancas.kern.core 'get-msg-str)) 


;; Utility functions.

(defn- get-class
  "Returns the class name of the parser state's value."
  [s] (.getName (class (:value s))))


;; +-------------------------------------------------------------+
;; |                       Basic lexers.                         |
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
  (let [s1 (parse (>> trim (many1 letter)) " \t\n\t\t ABC")]
    (fact "trim - some whitespace before letters"
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
  (let [s1 (parse (lexeme (many1 letter)) "ABC \t\n\t\t")]
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
  (let [s1 (parse (one-of "+-*/") "+ \n\t4;")]
    (fact "one-of - parses one operator"
	  (:value s1)  =>  \+
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\4 \;]
	  (:empty s1)  =>  false)))


(deftest test-0055
  (let [s1 (parse (one-of "+-*/") "3 + 4;")
    	em (get-msg-str (:error s1))]
    (fact "one-of - fails when there's no such operators"
	  (:input s1)  =>  (seq "3 + 4;")
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\3")))


(deftest test-0060
  (let [s1 (parse (none-of "+-*/") "> \n\t4;")]
    (fact "none-of - parses none of these operators"
	  (:value s1)  =>  \>
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\4 \;]
	  (:empty s1)  =>  false)))


(deftest test-0065
  (let [s1 (parse (none-of "+-*/") "+ 4;")
    	em (get-msg-str (:error s1))]
    (fact "none-of - fails when there's one of these operators"
	  (:input s1)  =>  (seq "+ 4;")
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\+")))


(deftest test-0070
  (let [in "program   := foo."
	s1 (parse (token "program") in)]
    (fact "token - parses a specific word, then trims whitespaces"
	  (:input s1)  =>  (seq ":= foo.")
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0075
  (let [in "program:= foo."
	s1 (parse (token "program") in)]
    (fact "token - parses a specific word"
	  (:input s1)  =>  (seq ":= foo.")
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0080
  (let [in "foo\t  (bar)\n\nbaz"
	s1 (parse (>>= (token "foo")
		       (fn [a]
		         (>>= (token "(bar)")
			      (fn [b]
				(>>= (token "baz")
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "token - three in a row ignoring whitespaces"
	  (:input s1)  =>  empty?
	  (:value s1) => ["foo" "(bar)" "baz"])))


(deftest test-0085
  (let [in "goat"
	s1 (parse (token "goal") in)
	em (get-msg-str (:error s1))]
    (fact "token - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected goat\nexpecting goal")))


(deftest test-0090
  (let [in "function    f()"
	s1 (parse (token "function" "procedure") in)]
    (fact "token - parses one of multiple word choices"
	  (:input s1)  =>  [\f \( \)]
	  (:value s1)  =>  "function"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0095
  (let [in "procedure    f()"
	s1 (parse (token "function" "procedure") in)]
    (fact "token - parses one of multiple word choices"
	  (:input s1)  =>  [\f \( \)]
	  (:value s1)  =>  "procedure"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0100
  (let [in "goat"
	s1 (parse (token "goal" "gol" "gal" "moat") in)
	em (get-msg-str (:error s1))]
    (fact "token - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected goat\nunexpected goa\nexpecting goal, gol, gal or moat")))


(deftest test-0105
  (let [in "program \t\t foo()"
	s1 (parse (word "program") in)]
    (fact "word - parses a specific, delimited word"
	  (:input s1)  =>  [\f \o \o \( \)]
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0110
  (let [in "else{}"
	s1 (parse (word "else") in)]
    (fact "word - parses a specific, delimited word"
	  (:input s1)  =>  [\{ \}]
	  (:value s1)  =>  "else"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0115
  (let [in "procedure"
	s1 (parse (word "proc") in)
	em (get-msg-str (:error s1))]
    (fact "word - fails because is not delimited"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected e\nexpecting end of proc")))


(deftest test-0120
  (let [in "foobar()"
	s1 (parse identifier in)]
    (fact "identifier - parses a basic identifier"
	  (:input s1)  =>  [\( \)]
	  (:value s1)  =>  "foobar"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0125
  (let [in "total_45   := 0;"
	s1 (parse identifier in)]
    (fact "identifier - parses a basic identifier"
	  (:input s1)  =>  [\: \= \space \0 \;]
	  (:value s1)  =>  "total_45"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0130
  (let [in "4privateUse"
	s1 (parse identifier in)
	em (get-msg-str (:error s1))]
    (fact "identifier - fails with invalid starting char"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\4\nexpecting letter or \\_")))


(deftest test-0135
  (let [in "'a' \t\t|;"
	s1 (parse char-lit in)]
    (fact "char-lit - parses a character literal"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0140
  (let [in "'\\n' \t\t|;"
	s1 (parse char-lit in)]
    (fact "char-lit - parses an escaped character literal"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0145
  (let [in "'\\t' \t\t|;"
	s1 (parse char-lit in)]
    (fact "char-lit - parses an escaped character literal"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0150
  (let [in "'u2"
	s1 (parse char-lit in)
	em (get-msg-str (:error s1))]
    (fact "char-lit - fails with a missing closing quote"
	  (:input s1)  =>  [\2]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\2\nexpecting end of character literal")))


(deftest test-0155
  (let [in "u2"
	s1 (parse char-lit in)
	em (get-msg-str (:error s1))]
    (fact "char-lit - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\u\nexpecting character literal")))


(deftest test-0160
  (let [in "\"now is the time\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a simple string literal"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "now is the time"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0165
  (let [in "\"now\\nis\\tthe\\ttime\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a string with escaped chars"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "now\nis\tthe\ttime"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0170
  (let [in "\"now is the time"
	s1 (parse string-lit in)
	em (get-msg-str (:error s1))]
    (fact "string-lit - fails with a string that is not terminated"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected end of input\nexpecting end of string literal")))


(deftest test-0175
  (let [in "45 + foobar"
	s1 (parse string-lit in)
	em (get-msg-str (:error s1))]
    (fact "string-lit - fails; not a string"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\4\nexpecting string literal")))


(deftest test-0180
  (let [in "+100"
	s1 (parse dec-lit in)]
    (fact "dec-lit - parses a redundant positive sign"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  100
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0185
  (let [in "123456789"
	s1 (parse dec-lit in)]
    (fact "dec-lit - parses an integer"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  123456789
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0190
  (let [in "1000000000N"
	s1 (parse dec-lit in)]
    (fact "dec-lit - parses a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1000000000N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0195
  (let [in "-747"
	s1 (parse dec-lit in)]
    (fact "dec-lit - parses a negative int"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0200
  (let [in "9999999999999999999"
	s1 (parse dec-lit in)
	c1 (get-class s1)]
    (fact "dec-lit - promotes a decimal to a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  9999999999999999999N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
                   c1  =>  "clojure.lang.BigInt")))

(deftest test-0205
  (let [in "100NA"
	s1 (parse dec-lit in)
	em (get-msg-str (:error s1))]
    (fact "dec-lit - fails; no letter can follow N"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected A\nexpecting decimal literal")))


(deftest test-0210
  (let [in "100e"
	s1 (parse dec-lit in)
	em (get-msg-str (:error s1))]
    (fact "dec-lit - fails; no letter can follow the last digit"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected e\nexpecting decimal literal")))


(deftest test-0215
  (let [in "99."
	s1 (parse dec-lit in)
	em (get-msg-str (:error s1))]
    (fact "dec-lit - fails; the last digit can't be followed by a dot"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected .\nexpecting decimal literal")))


(deftest test-0220
  (let [in "0"
	s1 (parse oct-lit in)]
    (fact "oct-lit - zero should be a valid octal number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0225
  (let [in "+0100"
	s1 (parse oct-lit in)]
    (fact "oct-lit - parses a redundant positive sign"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  64
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0230
  (let [in "012345676543210"
	s1 (parse oct-lit in)]
    (fact "oct-lit - parses an integer"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  012345676543210
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0235
  (let [in "03777N"
	s1 (parse oct-lit in)]
    (fact "oct-lit - parses a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  03777N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0240
  (let [in "-0747"
	s1 (parse oct-lit in)]
    (fact "oct-lit - parses a negative int"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -0747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0245
  (let [in "055555555555555555555N"
	s1 (parse oct-lit in)
	c1 (get-class s1)]
    (fact "oct-lit - promotes a decimal to a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  055555555555555555555N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
                   c1  =>  "clojure.lang.BigInt")))

(deftest test-0250
  (let [in "0100NA"
	s1 (parse oct-lit in)
	em (get-msg-str (:error s1))]
    (fact "oct-lit - fails; no letter can follow N"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected A\nexpecting octal literal")))


(deftest test-0255
  (let [in "0100e"
	s1 (parse oct-lit in)
	em (get-msg-str (:error s1))]
    (fact "oct-lit - fails; no letter can follow the last digit"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected e\nexpecting octal literal")))


(deftest test-0260
  (let [in "077."
	s1 (parse oct-lit in)
	em (get-msg-str (:error s1))]
    (fact "oct-lit - fails; the last digit can't be followed by a dot"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected .\nexpecting octal literal")))


(deftest test-0265
  (let [in "0x0"
	s1 (parse hex-lit in)]
    (fact "hex-lit - parses a zero"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0270
  (let [in "0XABCDEF"
	s1 (parse hex-lit in)]
    (fact "hex-lit - uses all capial letters"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0xabcdef
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0275
  (let [in "0xabcdef"
	s1 (parse hex-lit in)]
    (fact "hex-lit - uses all capial letters"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0XABCDEF
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280
  (let [in "+0x100"
	s1 (parse hex-lit in)]
    (fact "hex-lit - parses a redundant positive sign"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0x100
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0285
  (let [in "0xCAFEBABE"
	s1 (parse hex-lit in)]
    (fact "hex-lit - parses an integer"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0xCAFEBABE
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0290
  (let [in "0x00008B55CABA0000N"
	s1 (parse hex-lit in)]
    (fact "hex-lit - parses a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0x00008B55CABA0000N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0295
  (let [in "-0x0F0E"
	s1 (parse hex-lit in)]
    (fact "hex-lit - parses a negative int"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -0x0F0E
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0300
  (let [in "0x8000FA7770000B3400400200A"
	s1 (parse hex-lit in)
	c1 (get-class s1)]
    (fact "hex-lit - promotes a decimal to a BigInt"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0x8000FA7770000B3400400200AN
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
                   c1  =>  "clojure.lang.BigInt")))

(deftest test-0305
  (let [in "0x100NA"
	s1 (parse hex-lit in)
	em (get-msg-str (:error s1))]
    (fact "hex-lit - fails; no letter can follow N"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected A\nexpecting hex literal")))


(deftest test-0310
  (let [in "0x100x"
	s1 (parse hex-lit in)
	em (get-msg-str (:error s1))]
    (fact "hex-lit - fails; no letter can follow the last digit"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected x\nexpecting hex literal")))


(deftest test-0315
  (let [in "0x77."
	s1 (parse hex-lit in)
	em (get-msg-str (:error s1))]
    (fact "hex-lit - fails; the last digit can't be followed by a dot"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected .\nexpecting hex literal")))


(deftest test-0320
  (let [in "0"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a zero"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0.0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0325
  (let [in "0.0"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a zero"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0.0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0330
  (let [in "+100.00"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a redundant positive sign"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  100.00
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0335
  (let [in "1558.95"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a floating-point number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1558.95
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0340
  (let [in "1558.955e12"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a floating-point number with an exponent"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1558.955e12
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0345
  (let [in "1558.9558e-12"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a floating-point number with an exponent"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1558.9558e-12
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0350
  (let [in "1558.9558e+12"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a floating-point number with an exponent"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1558.9558e+12
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0355
  (let [in "-1558.9558e-12"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a negative floating-point number with an exponent"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -1558.9558e-12
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0360
  (let [in "1558e12"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a number with no fractional part and an exponent"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  1558e12
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0365
  (let [in "3.1415927M"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a BigDecimal"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  3.1415927M
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0370
  (let [in "-199.95"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a negative floating-point number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -199.95
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0370-05
  (let [in "999"
	s1 (parse float-lit in)]
    (fact "float-lit - parses a round integer as a floating-point number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  999.0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0370-10
  (let [in "999"
	s1 (parse float-lit in)]
    (fact "float-lit - regression test: number should not be long"
	  (:value s1)  =not=>  999)))


(deftest test-0375
  (let [in "99.95MA"
	s1 (parse float-lit in)
	em (get-msg-str (:error s1))]
    (fact "float-lit - fails; no letter can follow M"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected A\nexpecting floating-point literal")))


(deftest test-0380
  (let [in "99.95X"
	s1 (parse float-lit in)
	em (get-msg-str (:error s1))]
    (fact "float-lit - fails; no letter can follow the last digit"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected X\nexpecting floating-point literal")))


(deftest test-0385
  (let [in ".9999"
	s1 (parse float-lit in)
	em (get-msg-str (:error s1))]
    (fact "float-lit - fails; cannot start with a dot"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\.\nexpecting floating-point literal")))


(deftest test-0390
  (let [in "true"
	s1 (parse bool-lit in)]
    (fact "bool-lit - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0395
  (let [in "false"
	s1 (parse bool-lit in)]
    (fact "bool-lit - false"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  false
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0400
  (let [in "true \t \t)"
	s1 (parse bool-lit in)]
    (fact "bool-lit - true with whitespace"
	  (:input s1)  =>  [\)]
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0405
  (let [in "false\n\t\t :"
	s1 (parse bool-lit in)]
    (fact "bool-lit - false with whitespace"
	  (:input s1)  =>  [\:]
	  (:value s1)  =>  false
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0410
  (let [in "nil"
	s1 (parse nil-lit in)]
    (fact "nil-lit - nil"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0415
  (let [in "null"
	s1 (parse nil-lit in)]
    (fact "nil-lit - null"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0420
  (let [in "nil \t \t)"
	s1 (parse nil-lit in)]
    (fact "nil-lit - nil with whitespace"
	  (:input s1)  =>  [\)]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0425
  (let [in "null\n\t\t :"
	s1 (parse nil-lit in)]
    (fact "nil-lit - null with whitespace"
	  (:input s1)  =>  [\:]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0430
  (let [in "(true)"
	s1 (parse (parens bool-lit) in)]
    (fact "parens - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0435
  (let [in "()"
	s1 (parse (parens (many dec-lit)) in)]
    (fact "parens - nothing"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0440
  (let [in "( 11 22 33 44 55 )"
	s1 (parse (parens (many1 dec-lit)) in)]
      (fact "parens - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [11 22 33 44 55]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0445
  (let [in "11 22 33)"
	s1 (parse (parens (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "parens - fails; no starting paren"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\1\nexpecting \\(")))


(deftest test-0450
  (let [in "(11 22 33 ;"
	s1 (parse (parens (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "parens - fails; no ending paren"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\;\nexpecting \\)")))


(deftest test-0455
  (let [in "()"
	s1 (parse (parens dec-lit) in)
	em (get-msg-str (:error s1))]
    (fact "parens - fails; missing value"
	  (:input s1)  =>  [\)]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\)\nexpecting decimal literal")))


(deftest test-0460
  (let [in "{true}"
	s1 (parse (braces bool-lit) in)]
    (fact "braces - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0465
  (let [in "{}"
	s1 (parse (braces (many dec-lit)) in)]
    (fact "braces - nothing"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0470
  (let [in "{ 11 22 33 44 55 }"
	s1 (parse (braces (many1 dec-lit)) in)]
      (fact "braces - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [11 22 33 44 55]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0475
  (let [in "11 22 33}"
	s1 (parse (braces (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "braces - fails; no starting brace"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\1\nexpecting \\{")))


(deftest test-0480
  (let [in "{11 22 33 ;"
	s1 (parse (braces (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "braces - fails; no ending brace"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\;\nexpecting \\}")))


(deftest test-0485
  (let [in "{}"
	s1 (parse (braces dec-lit) in)
	em (get-msg-str (:error s1))]
    (fact "braces - fails; missing value"
	  (:input s1)  =>  [\}]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\}\nexpecting decimal literal")))


(deftest test-0490
  (let [in "<true>"
	s1 (parse (angles bool-lit) in)]
    (fact "angles - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0495
  (let [in "<>"
	s1 (parse (angles (many dec-lit)) in)]
    (fact "angles - nothing"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0500
  (let [in "< 11 22 33 44 55 >"
	s1 (parse (angles (many1 dec-lit)) in)]
      (fact "angles - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [11 22 33 44 55]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0505
  (let [in "11 22 33>"
	s1 (parse (angles (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "angles - fails; no starting angle bracket"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\1\nexpecting \\<")))


(deftest test-0510
  (let [in "<11 22 33 ;"
	s1 (parse (angles (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "angles - fails; no ending angle bracket"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\;\nexpecting \\>")))


(deftest test-0515
  (let [in "<>"
	s1 (parse (angles dec-lit) in)
	em (get-msg-str (:error s1))]
    (fact "angles - fails; missing value"
	  (:input s1)  =>  [\>]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\>\nexpecting decimal literal")))


(deftest test-0520
  (let [in "[true]"
	s1 (parse (brackets bool-lit) in)]
    (fact "brackets - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  true
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0525
  (let [in "[]"
	s1 (parse (brackets (many dec-lit)) in)]
    (fact "brackets - nothing"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0530
  (let [in "[ 11 22 33 44 55 ]"
	s1 (parse (brackets (many1 dec-lit)) in)]
      (fact "brackets - true"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [11 22 33 44 55]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0535
  (let [in "11 22 33]"
	s1 (parse (brackets (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "brackets - fails; no starting bracket"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\1\nexpecting \\[")))


(deftest test-0540
  (let [in "[11 22 33 ;"
	s1 (parse (brackets (many1 dec-lit)) in)
	em (get-msg-str (:error s1))]
    (fact "brackets - fails; no ending bracket"
	  (:input s1)  =>  [\;]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\;\nexpecting \\]")))


(deftest test-0545
  (let [in "[]"
	s1 (parse (brackets dec-lit) in)
	em (get-msg-str (:error s1))]
    (fact "brackets - fails; missing value"
	  (:input s1)  =>  [\]]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\]\nexpecting decimal literal")))


(deftest test-0550
  (let [in ";\n\n"
	s1 (parse semi in)]
      (fact "semi"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \;
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0555
  (let [in "+"
	s1 (parse semi in)
	em (get-msg-str (:error s1))]
    (fact "semi - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\+\nexpecting \\;")))


(deftest test-0560
  (let [in ",\n\n"
	s1 (parse comma in)]
      (fact "comma"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \,
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0565
  (let [in "+"
	s1 (parse comma in)
	em (get-msg-str (:error s1))]
    (fact "comma - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\+\nexpecting \\,")))


(deftest test-0570
  (let [in ":\n\n"
	s1 (parse colon in)]
      (fact "colon"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \:
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0575
  (let [in "+"
	s1 (parse colon in)
	em (get-msg-str (:error s1))]
    (fact "colon - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\+\nexpecting \\:")))


(deftest test-0580
  (let [in ".\n\n"
	s1 (parse dot in)]
      (fact "comma"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \.
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0585
  (let [in "+"
	s1 (parse dot in)
	em (get-msg-str (:error s1))]
    (fact "dot - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\+\nexpecting \\.")))


(deftest test-0590
  (let [s1 (parse (semi-sep digit) "*")]
    (fact "semi-sep - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0595
  (let [s1 (parse (semi-sep (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "semi-sep - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0600
  (let [s1 (parse (semi-sep digit) "0*")]
    (fact "semi-sep - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0605
  (let [s1 (parse (semi-sep (<*> upper digit)) "U2*")]
    (fact "semi-sep - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0610
  (let [s1 (parse (semi-sep (>> letter digit)) "U2;*")
    	em (get-msg-str (:error s1))]
    (fact "semi-sep - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter")))


(deftest test-0615
  (let [s1 (parse (semi-sep dec-lit) "550; 101*")]
    (fact "semi-sep - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [550 101]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0620
  (let [s1 (parse (semi-sep hex-lit) "+0xFADE  ; -0x7800 *")]
    (fact "semi-sep - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [0xFADE -0x7800]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0625
  (let [p1 (semi-sep identifier)
	s1 (parse p1 "one ;\n   two ; \t\t  three")]
    (fact "semi-sep - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  ["one" "two" "three"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0630
  (let [s1 (parse (semi-sep1 digit) "*")
	em (get-msg-str (:error s1))]
    (fact "semi-sep1 - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0635
  (let [s1 (parse (semi-sep1 (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "semi-sep1 - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0640
  (let [s1 (parse (semi-sep1 dec-lit) "747*")]
    (fact "semi-sep1 - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [747]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0645
  (let [s1 (parse (semi-sep1 (<*> upper digit)) "U2*")]
    (fact "semi-sep1 - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0650
  (let [s1 (parse (semi-sep1 identifier) "U2;*")
    	em (get-msg-str (:error s1))]
    (fact "semi-sep1 - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter or \\_")))


(deftest test-0655
  (let [s1 (parse (semi-sep1 dec-lit) "100;200*")]
    (fact "semi-sep1 - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [100 200]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0660
  (let [s1 (parse (semi-sep1 dec-lit) "-100 \t \t;\n +200*")]
    (fact "semi-sep1 - two compound items with whitespace"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [-100 200]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0665
  (let [p1 (semi-sep1 identifier)
	s1 (parse p1 "one ;\n\n two \t\t;\n\t  three")]
    (fact "semi-sep1 - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  ["one" "two" "three"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0670
  (let [s1 (parse (semi-sep1 (>> upper digit)) "A1; B2;  \tC3;DD;*")
    	em (get-msg-str (:error s1))]
    (fact "semi-sep1 - compound item fails after reading several items"
	  (:input s1)  =>  [\D \; \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\D\nexpecting digit")))


(deftest test-0675
  (let [s1 (parse (comma-sep digit) "*")]
    (fact "comma-sep - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0680
  (let [s1 (parse (comma-sep (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "comma-sep - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0685
  (let [s1 (parse (comma-sep digit) "0*")]
    (fact "comma-sep - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0690
  (let [s1 (parse (comma-sep (<*> upper digit)) "U2*")]
    (fact "comma-sep - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0695
  (let [s1 (parse (comma-sep (>> letter digit)) "U2,*")
    	em (get-msg-str (:error s1))]
    (fact "comma-sep - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter")))


(deftest test-0700
  (let [s1 (parse (comma-sep dec-lit) "550, 101*")]
    (fact "comma-sep - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [550 101]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0705
  (let [s1 (parse (comma-sep hex-lit) "+0xFADE  , -0x7800 *")]
    (fact "comma-sep - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [0xFADE -0x7800]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0710
  (let [p1 (comma-sep identifier)
	s1 (parse p1 "one ,\n   two , \t\t  three")]
    (fact "comma-sep - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  ["one" "two" "three"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0715
  (let [s1 (parse (comma-sep1 digit) "*")
	em (get-msg-str (:error s1))]
    (fact "comma-sep1 - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0720
  (let [s1 (parse (comma-sep1 (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "comma-sep1 - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0725
  (let [s1 (parse (comma-sep1 dec-lit) "747*")]
    (fact "comma-sep1 - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [747]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0730
  (let [s1 (parse (comma-sep1 (<*> upper digit)) "U2*")]
    (fact "comma-sep1 - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0735
  (let [s1 (parse (comma-sep1 identifier) "U2,*")
    	em (get-msg-str (:error s1))]
    (fact "comma-sep1 - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter or \\_")))


(deftest test-0740
  (let [s1 (parse (comma-sep1 dec-lit) "100,200*")]
    (fact "comma-sep1 - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [100 200]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0745
  (let [s1 (parse (comma-sep1 dec-lit) "-100 \t \t,\n +200*")]
    (fact "comma-sep1 - two compound items with whitespace"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [-100 200]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0750
  (let [p1 (comma-sep1 identifier)
	s1 (parse p1 "one ,\n\n two \t\t,\n\t  three")]
    (fact "comma-sep1 - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  ["one" "two" "three"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0755
  (let [s1 (parse (comma-sep1 (>> upper digit)) "A1, B2,  \tC3,DD,*")
    	em (get-msg-str (:error s1))]
    (fact "comma-sep1 - compound item fails after reading several items"
	  (:input s1)  =>  [\D \, \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\D\nexpecting digit")))


(deftest test-0760
  (let [s1 (parse char-lit "'z'")]
    (fact "char-lit"
	  (:value s1)  =>  \z
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0765
  (let [s1 (parse char-lit "'\\b'")]
    (fact "char-lit"
	  (:value s1)  =>  \backspace
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0770
  (let [s1 (parse char-lit "'\\t'")]
    (fact "char-lit"
	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0775
  (let [s1 (parse char-lit "'\\n'")]
    (fact "char-lit"
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0780
  (let [s1 (parse char-lit "'\\f'")]
    (fact "char-lit"
	  (:value s1)  =>  \formfeed
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0785
  (let [s1 (parse char-lit "'\\r'")]
    (fact "char-lit"
	  (:value s1)  =>  \return
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0790
  (let [s1 (parse char-lit "'\\''")]
    (fact "char-lit"
	  (:value s1)  =>  \'
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0795
  (let [s1 (parse char-lit "'\\\"'")]
    (fact "char-lit"
	  (:value s1)  =>  \"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0800
  (let [s1 (parse char-lit "'\\\\'")]
    (fact "char-lit"
	  (:value s1)  =>  \\
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0805
  (let [s1 (parse char-lit "'\\/'")]
    (fact "char-lit"
	  (:value s1)  =>  \/
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))

(deftest test-0810
  (let [in "'a "
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (fact "char-lit - fails without the closing quote"
	  (:input s1)  =>  [\space]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\space\nexpecting end of character literal")))


(deftest test-0815
  (let [in "'\\n "
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (fact "char-lit - fails without the closing quote"
	  (:input s1)  =>  [\space]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\space\nexpecting end of character literal")))


(deftest test-0820
  (let [in "\"\\bnow\\tis\\nthe\\ftime\\r\" \t\t|;"
	s1 (parse string-lit in)]
    (fact "string-lit - parses a string with multiple escaped characters"
	  (:input s1)  =>  [\| \;]
	  (:value s1)  =>  "\bnow\tis\nthe\ftime\r"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


;; +-------------------------------------------------------------+
;; |                      Custom lexers.                         |
;; +-------------------------------------------------------------+


(deftest test-0820
  (let [rec (lex/make-parsers (assoc lex/shell-style :line-continuation (sym* \?)))]
    (lex/with-parsers rec
      (let [in "foo bar foobar ?\n\t\tbaz"
	    s1 (parse (many1 lex/identifier) in)]
        (fact "trim - line continuation with a ?"
	      (:input s1)  =>  empty?
	      (:value s1)  =>  ["foo" "bar" "foobar" "baz"]
	      (:ok    s1)  =>  true
	      (:empty s1)  =>  false)))))


(deftest test-0825
  (let [rec (lex/make-parsers (assoc lex/c-style :case-sensitive false))]
    (lex/with-parsers rec
      (let [in "WhIlE (..."
	    s1 (parse (lex/word "while") in)]
        (fact "word - non-case-sensitive"
	      (:input s1)  =>  [\( \. \. \.]
	      (:value s1)  =>  "while"
	      (:ok    s1)  =>  true
	      (:empty s1)  =>  false)))))


(deftest test-0830
  (let [rec (lex/make-parsers (assoc lex/c-style :nested-comments true))]
    (lex/with-parsers rec
      (let [in "foo /***outter /* inner comment */ end of outer***/"
	    s1 (parse lex/identifier in)]
        (fact "word - nested comments"
	      (:input s1)  =>  empty?
	      (:value s1)  =>  "foo"
	      (:ok    s1)  =>  true
	      (:empty s1)  =>  false)))))


(deftest test-0845
  (let [rec (lex/make-parsers (assoc lex/c-style :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "foobar * baz"
	    s1 (parse lex/identifier in)]
        (fact "word - valid word, not a reserved word"
	      (:value s1)  =>  "foobar"
	      (:ok    s1)  =>  true
	      (:error s1)  =>  nil
	      (:input s1)  =>  [\* \space \b \a \z]
	      (:empty s1)  =>  false)))))


(deftest test-0850
  (let [rec (lex/make-parsers (assoc lex/c-style :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "foo * bar"
	    s1 (parse lex/identifier in)
    	    em (get-msg-str (:error s1))]
        (fact "word - fails on a reserved word"
	      (:input s1)  =>  [\f \o \o \space \* \space \b \a \r]
	      (:value s1)  =>  nil
	      (:ok    s1)  =>  false
	      (:empty s1)  =>  true
	               em  => "foo is a reserved name")))))


(deftest test-0855
  (let [rec (lex/make-parsers (assoc lex/c-style
				     :case-sensitive false
				     :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "FOO * bar"
	    s1 (parse lex/identifier in)
    	    em (get-msg-str (:error s1))]
        (fact "word - fails on a reserved word"
	      (:input s1)  =>  [\F \O \O \space \* \space \b \a \r]
	      (:value s1)  =>  nil
	      (:ok    s1)  =>  false
	      (:empty s1)  =>  true
	               em  => "FOO is a reserved name")))))


;; +-------------------------------------------------------------+
;; |                    Repeating Patterns.                      |
;; +-------------------------------------------------------------+


(deftest test-0900
  (let [s1 (parse identifier "f")]
    (fact "identifier -- state must not be empty"
	  (:value s1)  =>  "f"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0905
  (let [s1 (parse (many identifier) "f")]
    (fact "identifier -- many should work with a single-letter id"
	  (:value s1)  =>  ["f"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0910
  (let [s1 (parse (comma-sep identifier) "f")]
    (fact "multiple identifier"
	  (:value s1)  =>  ["f"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0915
  (let [s1 (parse (many (sym \Q)) "QQQ")]
    (fact "multiple symbols"
	  (:value s1)  =>  [\Q \Q \Q]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0920
  (let [s1 (parse (many (one-of "abc")) "a b c")]
    (fact "many one-of"
	  (:value s1)  =>  [\a \b \c]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0925
  (let [s1 (parse (many (none-of "abc")) "xyz")]
    (fact "many none-of"
	  (:value s1)  =>  [\x \y \z]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0930
  (let [s1 (parse (many (token "abc")) "abcabc abc")]
    (fact "many token"
	  (:value s1)  =>  ["abc" "abc" "abc"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0935
  (let [s1 (parse (many0 (token "abc")) "xxx")]
    (fact "many0, nothing match but empty is cleared"
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  (seq "xxx")
	  (:empty s1)  =>  false)))


(deftest test-0940
  (let [s1 (parse (many (field " ")) "now is the time")]
    (fact "many field"
	  (:value s1)  =>  ["now" "is" "the" "time"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0945
  (let [s1 (parse (many (field " ")) "x y")]
    (fact "many token"
	  (:value s1)  =>  ["x" "y"]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0950
  (let [s1 (parse (many dec-lit) "0")]
    (fact "many dec-lit"
	  (:value s1)  =>  [0]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0955
  (let [s1 (parse (many oct-lit) "01")]
    (fact "many oct-lit"
	  (:value s1)  =>  [1]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0960
  (let [s1 (parse (many hex-lit) "0x1")]
    (fact "many hex-lit"
	  (:value s1)  =>  [1]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0965
  (let [s1 (parse (many float-lit) "1")]
    (fact "many float-lit"
	  (:value s1)  =>  [1.0]
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))
