;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, fro-m this software.

(ns blancas.kern.test-lexer
  (:require [blancas.kern.core :as k :refer [parse >> >>= <*>]]
            [blancas.kern.lexer.basic :as lex-basic]
            [clojure.test :refer [deftest is testing]]
            [blancas.kern.lexer :as lex])
  #?(:cljs (:require-macros [blancas.kern.lexer :as lex])))

;; Utility functions.

(defn- get-class
  "Returns the class name of the parser state's value."
  [s]  (str (type (:value s))))


;; +-------------------------------------------------------------+
;; |                       Basic lexers.                         |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> lex-basic/trim k/eof) "  \t\t\n")]
    (testing "trim - blank, tab, eol, then eof"
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0005
  (let [s1 (parse (>> lex-basic/trim (k/many k/digit)) "123")]
    (testing "trim - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0010
  (let [s1 (parse (>> lex-basic/trim (k/many1 k/letter)) " \t\n\t\t ABC")]
    (testing "trim - some whitespace before letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0015
  (let [s1 (parse (>> (lex-basic/lexeme (k/sym* \space)) k/eof) "  \t\t\n")]
    (testing "lexeme - a blank, then tab, eol; then eof"
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0020
  (let [s1 (parse (lex-basic/lexeme (k/many k/digit)) "123")]
    (testing "lexeme - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0025
  (let [s1 (parse (lex-basic/lexeme (k/many1 k/letter)) "ABC \t\n\t\t")]
    (testing "lexeme - some whitespace after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0030
  (let [s1 (parse lex-basic/new-line "\nfoo")]
    (testing "new-line - parses a new line and stops"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0035
  (let [s1 (parse lex-basic/new-line "\n\t\t   foo")]
    (testing "new-line - skip a new line and any other whitespace that follows"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0040
  (let [s1 (parse lex-basic/new-line "\r\nfoo")]
    (testing "new-line - parses a Windows new-line and stops"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0045
  (let [s1 (parse lex-basic/new-line "foo")
        em (k/get-msg-str (:error s1))]
    (testing "new-line - fails when there's no new-line"
      (is (= [\f \o \o] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\f\nexpecting new line"
                :cljs "unexpected \"f\"\nexpecting new line")
             em)))))


(deftest test-0050
  (let [s1 (parse (lex-basic/one-of "+-*/") "+ \n\t4;")]
    (testing "one-of - parses one operator"
      (is (= \+ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\4 \;] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0055
  (let [s1 (parse (lex-basic/one-of "+-*/") "3 + 4;")
        em (k/get-msg-str (:error s1))]
    (testing "one-of - fails when there's no such operators"
      (is (= (seq "3 + 4;") (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\3"
                :cljs "unexpected \"3\"")
             em)))))


(deftest test-0060
  (let [s1 (parse (lex-basic/none-of "+-*/") "> \n\t4;")]
    (testing "none-of - parses none of these operators"
      (is (= \> (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\4 \;] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0065
  (let [s1 (parse (lex-basic/none-of "+-*/") "+ 4;")
        em (k/get-msg-str (:error s1))]
    (testing "none-of - fails when there's one of these operators"
      (is (= (seq "+ 4;") (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\+"
                :cljs "unexpected \"+\"")
             em)))))


(deftest test-0070
  (let [in "program   := foo."
        s1 (parse (lex-basic/token "program") in)]
    (testing "token - parses a specific word, then trims whitespaces"
      (is (= (seq ":= foo.") (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0075
  (let [in "program:= foo."
        s1 (parse (lex-basic/token "program") in)]
    (testing "token - parses a specific word"
      (is (= (seq ":= foo.") (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0080
  (let [in "foo\t  (bar)\n\nbaz"
        s1 (parse (>>= (lex-basic/token "foo")
                       (fn [a]
                         (>>= (lex-basic/token "(bar)")
                              (fn [b]
                                (>>= (lex-basic/token "baz")
                                     (fn [c]
                                       (k/return [a b c])))))))
                  in)]
    (testing "token - three in a row ignoring whitespaces"
      (is (empty? (:input s1)))
      (is (= ["foo" "(bar)" "baz"]))) (:value s1)))


(deftest test-0085
  (let [in "goat"
        s1 (parse (lex-basic/token "goal") in)
        em (k/get-msg-str (:error s1))]
    (testing "token - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected goat\nexpecting goal" em)))))


(deftest test-0090
  (let [in "function    f()"
        s1 (parse (lex-basic/token "function" "procedure") in)]
    (testing "token - parses one of multiple word choices"
      (is (= [\f \( \)] (:input s1)))
      (is (= "function" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0095
  (let [in "procedure    f()"
        s1 (parse (lex-basic/token "function" "procedure") in)]
    (testing "token - parses one of multiple word choices"
      (is (= [\f \( \)] (:input s1)))
      (is (= "procedure" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0100
  (let [in "goat"
        s1 (parse (lex-basic/token "goal" "gol" "gal" "moat") in)
        em (k/get-msg-str (:error s1))]
    (testing "token - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected goat\nunexpected goa\nexpecting goal, gol, gal or moat" em)))))


(deftest test-0105
  (let [in "program \t\t foo()"
        s1 (parse (lex-basic/word "program") in)]
    (testing "word - parses a specific, delimited word"
      (is (= [\f \o \o \( \)] (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0110
  (let [in "else{}"
        s1 (parse (lex-basic/word "else") in)]
    (testing "word - parses a specific, delimited word"
      (is (= [\{ \}] (:input s1)))
      (is (= "else" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0115
  (let [in "procedure"
        s1 (parse (lex-basic/word "proc") in)
        em (k/get-msg-str (:error s1))]
    (testing "word - fails because is not delimited"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected e\nexpecting end of proc" em)))))


(deftest test-0120
  (let [in "foobar()"
        s1 (parse lex-basic/identifier in)]
    (testing "identifier - parses a basic identifier"
      (is (= [\( \)] (:input s1)))
      (is (= "foobar" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0125
  (let [in "total_45   := 0;"
        s1 (parse lex-basic/identifier in)]
    (testing "identifier - parses a basic identifier"
      (is (= [\: \= \space \0 \;] (:input s1)))
      (is (= "total_45" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0130
  (let [in "4privateUse"
        s1 (parse lex-basic/identifier in)
        em (k/get-msg-str (:error s1))]
    (testing "identifier - fails with invalid starting char"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\4\nexpecting letter or \\_"
                :cljs "unexpected \"4\"\nexpecting letter or \"_\"")
             em)))))


(deftest test-0135
  (let [in "'a' \t\t|;"
        s1 (parse lex-basic/char-lit in)]
    (testing "char-lit - parses a character literal"
      (is (= [\| \;] (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0140
  (let [in "'\\n' \t\t|;"
        s1 (parse lex-basic/char-lit in)]
    (testing "char-lit - parses an escaped character literal"
      (is (= [\| \;] (:input s1)))
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))



(deftest test-0145
  (let [in "'\\t' \t\t|;"
        s1 (parse lex-basic/char-lit in)]
    (testing "char-lit - parses an escaped character literal"
      (is (= [\| \;] (:input s1)))
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0150
  (let [in "'u2"
        s1 (parse lex-basic/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails with a missing closing quote"
      (is (= [\2] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\2\nexpecting end of character literal"
                :cljs "unexpected \"2\"\nexpecting end of character literal")
             em)))))


(deftest test-0155
  (let [in "u2"
        s1 (parse lex-basic/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\u\nexpecting character literal"
                :cljs "unexpected \"u\"\nexpecting character literal")
             em)))))


(deftest test-0160
  (let [in "\"now is the time\" \t\t|;"
        s1 (parse lex-basic/string-lit in)]
    (testing "string-lit - parses a simple string literal"
      (is (= [\| \;] (:input s1)))
      (is (= "now is the time" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0165
  (let [in "\"now\\nis\\tthe\\ttime\" \t\t|;"
        s1 (parse lex-basic/string-lit in)]
    (testing "string-lit - parses a string with escaped chars"
      (is (= [\| \;] (:input s1)))
      (is (= "now\nis\tthe\ttime" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0170
  (let [in "\"now is the time"
        s1 (parse lex-basic/string-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "string-lit - fails with a string that is not terminated"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "unexpected end of input\nexpecting end of string literal" em)))))


(deftest test-0175
  (let [in "45 + foobar"
        s1 (parse lex-basic/string-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "string-lit - fails; not a string"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\4\nexpecting string literal"
                :cljs "unexpected \"4\"\nexpecting string literal") em)))))


(deftest test-0180
  (let [in "+100"
        s1 (parse lex-basic/dec-lit in)]
    (testing "dec-lit - parses a redundant positive sign"
      (is (empty? (:input s1)))
      (is (= 100 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0185
  (let [in "123456789"
        s1 (parse lex-basic/dec-lit in)]
    (testing "dec-lit - parses an integer"
      (is (empty? (:input s1)))
      (is (= 123456789 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0190
  (let [in "1000000000N"
        s1 (parse lex-basic/dec-lit in)]
    (testing "dec-lit - parses a BigInt"
      (is (empty? (:input s1)))
      (is (= 1000000000N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0195
  (let [in "-747"
        s1 (parse lex-basic/dec-lit in)]
    (testing "dec-lit - parses a negative int"
      (is (empty? (:input s1)))
      (is (= -747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0200
  (let [in "9999999999999999999"
        s1 (parse lex-basic/dec-lit in)
        c1 (get-class s1)]
    (testing "dec-lit - promotes a decimal to a BigInt"
      (is (empty? (:input s1)))
      (is (= 9999999999999999999N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (= c1 #?(:clj "class clojure.lang.BigInt" :cljs "function Number() { [native code] }"))))))

(deftest test-0205
  (let [in "100NA"
        s1 (parse lex-basic/dec-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "dec-lit - fails; no letter can follow N"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected A\nexpecting decimal literal" em)))))


(deftest test-0210
  (let [in "100e"
        s1 (parse lex-basic/dec-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "dec-lit - fails; no letter can follow the last digit"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected e\nexpecting decimal literal" em)))))


(deftest test-0215
  (let [in "99."
        s1 (parse lex-basic/dec-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "dec-lit - fails; the last digit can't be followed by a dot"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected .\nexpecting decimal literal" em)))))


(deftest test-0220
  (let [in "0"
        s1 (parse lex-basic/oct-lit in)]
    (testing "oct-lit - zero should be a valid octal number"
      (is (empty? (:input s1)))
      (is (= 0 (:value s1)))
      (is (:ok s1))
      (is (:empty s1)))))


(deftest test-0225
  (let [in "+0100"
        s1 (parse lex-basic/oct-lit in)]
    (testing "oct-lit - parses a redundant positive sign"
      (is (empty? (:input s1)))
      (is (= 64 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0230
  (let [in "012345676543210"
        s1 (parse lex-basic/oct-lit in)]
    (testing "oct-lit - parses an integer"
      (is (empty? (:input s1)))
      (is (= 012345676543210 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0235
  (let [in "03777N"
        s1 (parse lex-basic/oct-lit in)]
    (testing "oct-lit - parses a BigInt"
      (is (empty? (:input s1)))
      (is (= 03777N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0240
  (let [in "-0747"
        s1 (parse lex-basic/oct-lit in)]
    (testing "oct-lit - parses a negative int"
      (is (empty? (:input s1)))
      (is (= -0747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0245
  (let [in "055555555555555555555N"
        s1 (parse lex-basic/oct-lit in)
        c1 (get-class s1)]
    (testing "oct-lit - promotes a decimal to a BigInt"
      (is (empty? (:input s1)))
      (is (= 055555555555555555555N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (= c1 #?(:clj "class clojure.lang.BigInt" :cljs "function Number() { [native code] }"))))))

(deftest test-0250
  (let [in "0100NA"
        s1 (parse lex-basic/oct-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "oct-lit - fails; no letter can follow N"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected A\nexpecting octal literal" em)))))


(deftest test-0255
  (let [in "0100e"
        s1 (parse lex-basic/oct-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "oct-lit - fails; no letter can follow the last digit"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected e\nexpecting octal literal" em)))))


(deftest test-0260
  (let [in "077."
        s1 (parse lex-basic/oct-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "oct-lit - fails; the last digit can't be followed by a dot"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected .\nexpecting octal literal" em)))))


(deftest test-0265
  (let [in "0x0"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - parses a zero"
      (is (empty? (:input s1)))
      (is (= 0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0270
  (let [in "0XABCDEF"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - uses all capial letters"
      (is (empty? (:input s1)))
      (is (= 0xabcdef (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0275
  (let [in "0xabcdef"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - uses all capial letters"
      (is (empty? (:input s1)))
      (is (= 0XABCDEF (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280
  (let [in "+0x100"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - parses a redundant positive sign"
      (is (empty? (:input s1)))
      (is (= 0x100 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0285
  (let [in "0xCAFEBABE"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - parses an integer"
      (is (empty? (:input s1)))
      (is (= 0xCAFEBABE (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0290
  (let [in "0x00008B55CABA0000N"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - parses a BigInt"
      (is (empty? (:input s1)))
      (is (= 0x00008B55CABA0000N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0295
  (let [in "-0x0F0E"
        s1 (parse lex-basic/hex-lit in)]
    (testing "hex-lit - parses a negative int"
      (is (empty? (:input s1)))
      (is (= -0x0F0E (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0300
  (let [in "0x8000FA7770000B3400400200A"
        s1 (parse lex-basic/hex-lit in)
        c1 (get-class s1)]
    (testing "hex-lit - promotes a decimal to a BigInt"
      (is (empty? (:input s1)))
      (is (= 0x8000FA7770000B3400400200AN (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (= c1 #?(:clj "class clojure.lang.BigInt" :cljs "function Number() { [native code] }"))))))

(deftest test-0305
  (let [in "0x100NA"
        s1 (parse lex-basic/hex-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "hex-lit - fails; no letter can follow N"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected A\nexpecting hex literal" em)))))


(deftest test-0310
  (let [in "0x100x"
        s1 (parse lex-basic/hex-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "hex-lit - fails; no letter can follow the last digit"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected x\nexpecting hex literal" em)))))


(deftest test-0315
  (let [in "0x77."
        s1 (parse lex-basic/hex-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "hex-lit - fails; the last digit can't be followed by a dot"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected .\nexpecting hex literal" em)))))


(deftest test-0320
  (let [in "0"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a zero"
      (is (empty? (:input s1)))
      (is (= 0.0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0325
  (let [in "0.0"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a zero"
      (is (empty? (:input s1)))
      (is (= 0.0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0330
  (let [in "+100.00"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a redundant positive sign"
      (is (empty? (:input s1)))
      (is (= 100.00 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0335
  (let [in "1558.95"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a floating-point number"
      (is (empty? (:input s1)))
      (is (= 1558.95 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0340
  (let [in "1558.955e12"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a floating-point number with an exponent"
      (is (empty? (:input s1)))
      (is (= 1558.955e12 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0345
  (let [in "1558.9558e-12"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a floating-point number with an exponent"
      (is (empty? (:input s1)))
      (is (= 1558.9558e-12 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0350
  (let [in "1558.9558e+12"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a floating-point number with an exponent"
      (is (empty? (:input s1)))
      (is (= 1558.9558e+12 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0355
  (let [in "-1558.9558e-12"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a negative floating-point number with an exponent"
      (is (empty? (:input s1)))
      (is (= -1558.9558e-12 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0360
  (let [in "1558e12"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a number with no fractional part and an exponent"
      (is (empty? (:input s1)))
      (is (= 1558e12 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0365
  (let [in "3.1415927M"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a BigDecimal"
      (is (empty? (:input s1)))
      (is (= 3.1415927M (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0370
  (let [in "-199.95"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a negative floating-point number"
      (is (empty? (:input s1)))
      (is (= -199.95 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0370-05
  (let [in "999"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - parses a round integer as a floating-point number"
      (is (empty? (:input s1)))
      (is (= 999.0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0370-10
  (let [in "999"
        s1 (parse lex-basic/float-lit in)]
    (testing "float-lit - regression test: number should not be long"
      (not (= 999 (:value s1))))))


(deftest test-0375
  (let [in "99.95MA"
        s1 (parse lex-basic/float-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "float-lit - fails; no letter can follow M"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected A\nexpecting floating-point literal" em)))))


(deftest test-0380
  (let [in "99.95X"
        s1 (parse lex-basic/float-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "float-lit - fails; no letter can follow the last digit"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected X\nexpecting floating-point literal" em)))))


(deftest test-0385
  (let [in ".9999"
        s1 (parse lex-basic/float-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "float-lit - fails; cannot start with a dot"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\.\nexpecting floating-point literal"
                :cljs "unexpected \".\"\nexpecting floating-point literal")
             em)))))


(deftest test-0390
  (let [in "true"
        s1 (parse lex-basic/bool-lit in)]
    (testing "bool-lit - true"
      (is (empty? (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0395
  (let [in "false"
        s1 (parse lex-basic/bool-lit in)]
    (testing "bool-lit - false"
      (is (empty? (:input s1)))
      (is (false? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0400
  (let [in "true \t \t)"
        s1 (parse lex-basic/bool-lit in)]
    (testing "bool-lit - true with whitespace"
      (is (= [\)] (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0405
  (let [in "false\n\t\t :"
        s1 (parse lex-basic/bool-lit in)]
    (testing "bool-lit - false with whitespace"
      (is (= [\:] (:input s1)))
      (is (false? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0410
  (let [in "nil"
        s1 (parse lex-basic/nil-lit in)]
    (testing "nil-lit - nil"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0415
  (let [in "null"
        s1 (parse lex-basic/nil-lit in)]
    (testing "nil-lit - null"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0420
  (let [in "nil \t \t)"
        s1 (parse lex-basic/nil-lit in)]
    (testing "nil-lit - nil with whitespace"
      (is (= [\)] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0425
  (let [in "null\n\t\t :"
        s1 (parse lex-basic/nil-lit in)]
    (testing "nil-lit - null with whitespace"
      (is (= [\:] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0430
  (let [in "(true)"
        s1 (parse (lex-basic/parens lex-basic/bool-lit) in)]
    (testing "parens - true"
      (is (empty? (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0435
  (let [in "()"
        s1 (parse (lex-basic/parens (k/many lex-basic/dec-lit)) in)]
    (testing "parens - nothing"
      (is (empty? (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0440
  (let [in "( 11 22 33 44 55 )"
        s1 (parse (lex-basic/parens (k/many1 lex-basic/dec-lit)) in)]
    (testing "parens - true"
      (is (empty? (:input s1)))
      (is (= [11 22 33 44 55] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0445
  (let [in "11 22 33)"
        s1 (parse (lex-basic/parens (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "parens - fails; no starting paren"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\1\nexpecting \\("
                :cljs "unexpected \"1\"\nexpecting \"(\"")
             em)))))


(deftest test-0450
  (let [in "(11 22 33 ;"
        s1 (parse (lex-basic/parens (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "parens - fails; no ending paren"
      (is (= [\;] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\;\nexpecting \\)"
                :cljs "unexpected \";\"\nexpecting \")\"")
             em)))))


(deftest test-0455
  (let [in "()"
        s1 (parse (lex-basic/parens lex-basic/dec-lit) in)
        em (k/get-msg-str (:error s1))]
    (testing "parens - fails; missing value"
      (is (= [\)] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\)\nexpecting decimal literal"
                :cljs "unexpected \")\"\nexpecting decimal literal")
             em)))))


(deftest test-0460
  (let [in "{true}"
        s1 (parse (lex-basic/braces lex-basic/bool-lit) in)]
    (testing "braces - true"
      (is (empty? (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0465
  (let [in "{}"
        s1 (parse (lex-basic/braces (k/many lex-basic/dec-lit)) in)]
    (testing "braces - nothing"
      (is (empty? (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0470
  (let [in "{ 11 22 33 44 55 }"
        s1 (parse (lex-basic/braces (k/many1 lex-basic/dec-lit)) in)]
    (testing "braces - true"
      (is (empty? (:input s1)))
      (is (= [11 22 33 44 55] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0475
  (let [in "11 22 33}"
        s1 (parse (lex-basic/braces (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "braces - fails; no starting brace"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\1\nexpecting \\{"
                :cljs "unexpected \"1\"\nexpecting \"{\"")
             em)))))


(deftest test-0480
  (let [in "{11 22 33 ;"
        s1 (parse (lex-basic/braces (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "braces - fails; no ending brace"
      (is (= [\;] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\;\nexpecting \\}"
                :cljs "unexpected \";\"\nexpecting \"}\"")
             em)))))


(deftest test-0485
  (let [in "{}"
        s1 (parse (lex-basic/braces lex-basic/dec-lit) in)
        em (k/get-msg-str (:error s1))]
    (testing "braces - fails; missing value"
      (is (= [\}] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\}\nexpecting decimal literal"
                :cljs "unexpected \"}\"\nexpecting decimal literal")
             em)))))


(deftest test-0490
  (let [in "<true>"
        s1 (parse (lex-basic/angles lex-basic/bool-lit) in)]
    (testing "angles - true"
      (is (empty? (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0495
  (let [in "<>"
        s1 (parse (lex-basic/angles (k/many lex-basic/dec-lit)) in)]
    (testing "angles - nothing"
      (is (empty? (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0500
  (let [in "< 11 22 33 44 55 >"
        s1 (parse (lex-basic/angles (k/many1 lex-basic/dec-lit)) in)]
    (testing "angles - true"
      (is (empty? (:input s1)))
      (is (= [11 22 33 44 55] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0505
  (let [in "11 22 33>"
        s1 (parse (lex-basic/angles (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "angles - fails; no starting angle bracket"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\1\nexpecting \\<"
                :cljs  "unexpected \"1\"\nexpecting \"<\"")
             em)))))


(deftest test-0510
  (let [in "<11 22 33 ;"
        s1 (parse (lex-basic/angles (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "angles - fails; no ending angle bracket"
      (is (= [\;] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\;\nexpecting \\>"
                :cljs "unexpected \";\"\nexpecting \">\"")
             em)))))


(deftest test-0515
  (let [in "<>"
        s1 (parse (lex-basic/angles lex-basic/dec-lit) in)
        em (k/get-msg-str (:error s1))]
    (testing "angles - fails; missing value"
      (is (= [\>] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\>\nexpecting decimal literal"
                :cljs "unexpected \">\"\nexpecting decimal literal")
             em)))))


(deftest test-0520
  (let [in "[true]"
        s1 (parse (lex-basic/brackets lex-basic/bool-lit) in)]
    (testing "brackets - true"
      (is (empty? (:input s1)))
      (is (:value s1))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0525
  (let [in "[]"
        s1 (parse (lex-basic/brackets (k/many lex-basic/dec-lit)) in)]
    (testing "brackets - nothing"
      (is (empty? (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0530
  (let [in "[ 11 22 33 44 55 ]"
        s1 (parse (lex-basic/brackets (k/many1 lex-basic/dec-lit)) in)]
    (testing "brackets - true"
      (is (empty? (:input s1)))
      (is (= [11 22 33 44 55] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0535
  (let [in "11 22 33]"
        s1 (parse (lex-basic/brackets (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "brackets - fails; no starting bracket"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\1\nexpecting \\["
                :cljs "unexpected \"1\"\nexpecting \"[\"")
             em)))))


(deftest test-0540
  (let [in "[11 22 33 ;"
        s1 (parse (lex-basic/brackets (k/many1 lex-basic/dec-lit)) in)
        em (k/get-msg-str (:error s1))]
    (testing "brackets - fails; no ending bracket"
      (is (= [\;] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\;\nexpecting \\]"
                :cljs "unexpected \";\"\nexpecting \"]\"")
             em)))))


(deftest test-0545
  (let [in "[]"
        s1 (parse (lex-basic/brackets lex-basic/dec-lit) in)
        em (k/get-msg-str (:error s1))]
    (testing "brackets - fails; missing value"
      (is (= [\]] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\]\nexpecting decimal literal"
                :cljs "unexpected \"]\"\nexpecting decimal literal")
             em)))))


(deftest test-0550
  (let [in ";\n\n"
        s1 (parse lex-basic/semi in)]
    (testing "semi"
      (is (empty? (:input s1)))
      (is (= \; (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0555
  (let [in "+"
        s1 (parse lex-basic/semi in)
        em (k/get-msg-str (:error s1))]
    (testing "semi - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\+\nexpecting \\;"
                :cljs "unexpected \"+\"\nexpecting \";\"")
             em)))))


(deftest test-0560
  (let [in ",\n\n"
        s1 (parse lex-basic/comma in)]
    (testing "comma"
      (is (empty? (:input s1)))
      (is (= \, (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0565
  (let [in "+"
        s1 (parse lex-basic/comma in)
        em (k/get-msg-str (:error s1))]
    (testing "comma - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\+\nexpecting \\,"
                :cljs "unexpected \"+\"\nexpecting \",\"")
             em)))))


(deftest test-0570
  (let [in ":\n\n"
        s1 (parse lex-basic/colon in)]
    (testing "colon"
      (is (empty? (:input s1)))
      (is (= \: (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0575
  (let [in "+"
        s1 (parse lex-basic/colon in)
        em (k/get-msg-str (:error s1))]
    (testing "colon - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\+\nexpecting \\:"
                :cljs "unexpected \"+\"\nexpecting \":\"")
             em)))))


(deftest test-0580
  (let [in ".\n\n"
        s1 (parse lex-basic/dot in)]
    (testing "comma"
      (is (empty? (:input s1)))
      (is (= \. (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0585
  (let [in "+"
        s1 (parse lex-basic/dot in)
        em (k/get-msg-str (:error s1))]
    (testing "dot - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\+\nexpecting \\."
                :cljs "unexpected \"+\"\nexpecting \".\"")
             em)))))


(deftest test-0590
  (let [s1 (parse (lex-basic/semi-sep k/digit) "*")]
    (testing "semi-sep - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0595
  (let [s1 (parse (lex-basic/semi-sep (>> k/letter k/digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0600
  (let [s1 (parse (lex-basic/semi-sep k/digit) "0*")]
    (testing "semi-sep - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0605
  (let [s1 (parse (lex-basic/semi-sep (<*> k/upper k/digit)) "U2*")]
    (testing "semi-sep - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0610
  (let [s1 (parse (lex-basic/semi-sep (>> k/letter k/digit)) "U2;*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter"
                :cljs "unexpected \"*\"\nexpecting letter")
             em)))))


(deftest test-0615
  (let [s1 (parse (lex-basic/semi-sep lex-basic/dec-lit) "550; 101*")]
    (testing "semi-sep - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [550 101] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0620
  (let [s1 (parse (lex-basic/semi-sep lex-basic/hex-lit) "+0xFADE  ; -0x7800 *")]
    (testing "semi-sep - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [0xFADE -0x7800] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0625
  (let [p1 (lex-basic/semi-sep lex-basic/identifier)
        s1 (parse p1 "one ;\n   two ; \t\t  three")]
    (testing "semi-sep - three compound items"
      (is (empty? (:input s1)))
      (is (= ["one" "two" "three"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0630
  (let [s1 (parse (lex-basic/semi-sep1 k/digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep1 - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0635
  (let [s1 (parse (lex-basic/semi-sep1 (>> k/letter k/digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep1 - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0640
  (let [s1 (parse (lex-basic/semi-sep1 lex-basic/dec-lit) "747*")]
    (testing "semi-sep1 - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [747] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0645
  (let [s1 (parse (lex-basic/semi-sep1 (<*> k/upper k/digit)) "U2*")]
    (testing "semi-sep1 - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0650
  (let [s1 (parse (lex-basic/semi-sep1 lex-basic/identifier) "U2;*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep1 - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter or \\_"
                :cljs  "unexpected \"*\"\nexpecting letter or \"_\"")
             em)))))


(deftest test-0655
  (let [s1 (parse (lex-basic/semi-sep1 lex-basic/dec-lit) "100;200*")]
    (testing "semi-sep1 - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [100 200] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0660
  (let [s1 (parse (lex-basic/semi-sep1 lex-basic/dec-lit) "-100 \t \t;\n +200*")]
    (testing "semi-sep1 - two compound items with whitespace"
      (is (= [\*] (:input s1)))
      (is (= [-100 200] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0665
  (let [p1 (lex-basic/semi-sep1 lex-basic/identifier)
        s1 (parse p1 "one ;\n\n two \t\t;\n\t  three")]
    (testing "semi-sep1 - three compound items"
      (is (empty? (:input s1)))
      (is (= ["one" "two" "three"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0670
  (let [s1 (parse (lex-basic/semi-sep1 (>> k/upper k/digit)) "A1; B2;  \tC3;DD;*")
        em (k/get-msg-str (:error s1))]
    (testing "semi-sep1 - compound item fails after reading several items"
      (is (= [\D \; \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\D\nexpecting digit"
                :cljs "unexpected \"D\"\nexpecting digit")
             em)))))


(deftest test-0675
  (let [s1 (parse (lex-basic/comma-sep k/digit) "*")]
    (testing "comma-sep - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0680
  (let [s1 (parse (lex-basic/comma-sep (>> k/letter k/digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0685
  (let [s1 (parse (lex-basic/comma-sep k/digit) "0*")]
    (testing "comma-sep - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0690
  (let [s1 (parse (lex-basic/comma-sep (<*> k/upper k/digit)) "U2*")]
    (testing "comma-sep - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0695
  (let [s1 (parse (lex-basic/comma-sep (>> k/letter k/digit)) "U2,*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter"
                :cljs "unexpected \"*\"\nexpecting letter")
             em)))))


(deftest test-0700
  (let [s1 (parse (lex-basic/comma-sep lex-basic/dec-lit) "550, 101*")]
    (testing "comma-sep - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [550 101] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0705
  (let [s1 (parse (lex-basic/comma-sep lex-basic/hex-lit) "+0xFADE  , -0x7800 *")]
    (testing "comma-sep - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [0xFADE -0x7800] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0710
  (let [p1 (lex-basic/comma-sep lex-basic/identifier)
        s1 (parse p1 "one ,\n   two , \t\t  three")]
    (testing "comma-sep - three compound items"
      (is (empty? (:input s1)))
      (is (= ["one" "two" "three"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0715
  (let [s1 (parse (lex-basic/comma-sep1 k/digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep1 - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0720
  (let [s1 (parse (lex-basic/comma-sep1 (>> k/letter k/digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep1 - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0725
  (let [s1 (parse (lex-basic/comma-sep1 lex-basic/dec-lit) "747*")]
    (testing "comma-sep1 - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [747] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0730
  (let [s1 (parse (lex-basic/comma-sep1 (<*> k/upper k/digit)) "U2*")]
    (testing "comma-sep1 - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0735
  (let [s1 (parse (lex-basic/comma-sep1 lex-basic/identifier) "U2,*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep1 - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter or \\_"
                :cljs "unexpected \"*\"\nexpecting letter or \"_\"")
             em)))))


(deftest test-0740
  (let [s1 (parse (lex-basic/comma-sep1 lex-basic/dec-lit) "100,200*")]
    (testing "comma-sep1 - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [100 200] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0745
  (let [s1 (parse (lex-basic/comma-sep1 lex-basic/dec-lit) "-100 \t \t,\n +200*")]
    (testing "comma-sep1 - two compound items with whitespace"
      (is (= [\*] (:input s1)))
      (is (= [-100 200] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0750
  (let [p1 (lex-basic/comma-sep1 lex-basic/identifier)
        s1 (k/parse p1 "one ,\n\n two \t\t,\n\t  three")]
    (testing "comma-sep1 - three compound items"
      (is (empty? (:input s1)))
      (is (= ["one" "two" "three"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0755
  (let [s1 (parse (lex-basic/comma-sep1 (>> k/upper k/digit)) "A1, B2,  \tC3,DD,*")
        em (k/get-msg-str (:error s1))]
    (testing "comma-sep1 - compound item fails after reading several items"
      (is (= [\D \, \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\D\nexpecting digit"
                :cljs "unexpected \"D\"\nexpecting digit")
             em)))))


(deftest test-0760
  (let [s1 (parse lex-basic/char-lit "'z'")]
    (testing "char-lit"
      (is (= \z (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0765
  (let [s1 (parse lex-basic/char-lit "'\\b'")]
    (testing "char-lit"
      (is (= \backspace (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0770
  (let [s1 (parse lex-basic/char-lit "'\\t'")]
    (testing "char-lit"
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0775
  (let [s1 (parse lex-basic/char-lit "'\\n'")]
    (testing "char-lit"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0780
  (let [s1 (parse lex-basic/char-lit "'\\f'")]
    (testing "char-lit"
      (is (= \formfeed (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0785
  (let [s1 (parse lex-basic/char-lit "'\\r'")]
    (testing "char-lit"
      (is (= \return (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0790
  (let [s1 (parse lex-basic/char-lit "'\\''")]
    (testing "char-lit"
      (is (= \' (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0795
  (let [s1 (parse lex-basic/char-lit "'\\\"'")]
    (testing "char-lit"
      (is (= \" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0800
  (let [s1 (parse lex-basic/char-lit "'\\\\'")]
    (testing "char-lit"
      (is (= \\ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0805
  (let [s1 (parse lex-basic/char-lit "'\\/'")]
    (testing "char-lit"
      (is (= \/ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))

(deftest test-0810
  (let [in "'a "
        s1 (parse lex-basic/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails without the closing quote"
      (is (= [\space] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\space\nexpecting end of character literal"
                :cljs "unexpected \" \"\nexpecting end of character literal")
             em)))))


(deftest test-0815
  (let [in "'\\n "
        s1 (parse lex-basic/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails without the closing quote"
      (is (= [\space] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\space\nexpecting end of character literal"
                :cljs "unexpected \" \"\nexpecting end of character literal")
             em)))))


(deftest test-0820
  (let [in "\"\\bnow\\tis\\nthe\\ftime\\r\" \t\t|;"
        s1 (parse lex-basic/string-lit in)]
    (testing "string-lit - parses a string with multiple escaped characters"
      (is (= [\| \;] (:input s1)))
      (is (= "\bnow\tis\nthe\ftime\r" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


;; +-------------------------------------------------------------+
;; |                      Custom lexers.                         |
;; +-------------------------------------------------------------+


(deftest test-0820-05
  (let [rec (lex/make-parsers (assoc lex/shell-style-def :line-continuation (k/sym* \?)))]
    (lex/with-parsers rec
      (let [in "foo bar foobar ?\n\t\tbaz"
            s1 (parse (k/many1 lex/identifier) in)]
        (testing "trim - line continuation with a ?"
          (is (empty? (:input s1)))
          (is (= ["foo" "bar" "foobar" "baz"] (:value s1)))
          (is (:ok s1))
          (is (false? (:empty s1))))))))


(deftest test-0825
  (let [rec (lex/make-parsers (assoc lex/c-style-def :case-sensitive false))]
    (lex/with-parsers rec
      (let [in "WhIlE (..."
            s1 (parse (lex/word "while") in)]
        (testing "word - non-case-sensitive"
          (is (= [\( \. \. \.] (:input s1)))
          (is (= "while" (:value s1)))
          (is (:ok s1))
          (is (false? (:empty s1))))))))


(deftest test-0830
  (let [rec (lex/make-parsers (assoc lex/c-style-def :nested-comments true))]
    (lex/with-parsers rec
      (let [in "foo /***outter /* inner comment */ end of outer***/"
            s1 (parse lex/identifier in)]
        (testing "word - nested comments"
          (is (empty? (:input s1)))
          (is (= "foo" (:value s1)))
          (is (:ok s1))
          (is (false? (:empty s1))))))))


(deftest test-0845
  (let [rec (lex/make-parsers (assoc lex/c-style-def :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "foobar * baz"
            s1 (parse lex/identifier in)]
        (testing "word - valid word, not a reserved word"
          (is (= "foobar" (:value s1)))
          (is (:ok s1))
          (is (nil? (:error s1)))
          (is (= [\* \space \b \a \z] (:input s1)))
          (is (false? (:empty s1))))))))


(deftest test-0850
  (let [rec (lex/make-parsers (assoc lex/c-style-def :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "foo * bar"
            s1 (parse lex/identifier in)
            em (k/get-msg-str (:error s1))]
        (testing "word - fails on a reserved word"
          (is (= [\f \o \o \space \* \space \b \a \r] (:input s1)))
          (is (nil? (:value s1)))
          (is (false? (:ok s1)))
          (is (:empty s1))
          (is (= "foo is a reserved name" em)))))))


(deftest test-0855
  (let [rec (lex/make-parsers (assoc lex/c-style-def
                                :case-sensitive false
                                :reserved-names ["foo" "bar"]))]
    (lex/with-parsers rec
      (let [in "FOO * bar"
            s1 (parse lex/identifier in)
            em (k/get-msg-str (:error s1))]
        (testing "word - fails on a reserved word"
          (is (= [\F \O \O \space \* \space \b \a \r] (:input s1)))
          (is (nil? (:value s1)))
          (is (false? (:ok s1)))
          (is (:empty s1))
          (is (= "FOO is a reserved name" em)))))))


(deftest test-0860
  (let [rec (lex/make-parsers (assoc lex/basic-def :leading-sign false))]
    (lex/with-parsers rec
      (let [in "5005"
            s1 (parse lex/dec-lit in)]
        (testing "dec-lit - regular case, no leading sign."
          (is (empty? (:input s1)))
          (is (= 5005 (:value s1)))
          (is (:ok s1))
          (is (false? (:empty s1))))))))


(deftest test-0865
  (let [rec (lex/make-parsers (assoc lex/basic-def :leading-sign false))]
    (lex/with-parsers rec
      (let [in "0xCAFE"
            s1 (parse lex/hex-lit in)]
        (testing "hex-lit - regular case, no leading sign."
          (is (empty? (:input s1)))
          (is (= 0xCAFE (:value s1)))
          (is (:ok s1))
          (is (false? (:empty s1))))))))


(deftest test-0870
  (let [rec (lex/make-parsers (assoc lex/basic-def :leading-sign false))]
    (lex/with-parsers rec
      (let [in "-5005"
            s1 (parse lex/dec-lit in)]
        (testing "dec-lit - with leading sign it fails."
          (is (= (seq "-5005") (:input s1)))
          (is (nil? (:value s1)))
          (is (false? (:ok s1))))))))


(deftest test-0875
  (let [rec (lex/make-parsers (assoc lex/basic-def :leading-sign false))]
    (lex/with-parsers rec
      (let [in "+3.1416"
            s1 (parse lex/float-lit in)]
        (testing "float-lit - with leading sign it fails."
          (is (= (seq "+3.1416") (:input s1)))
          (is (nil? (:value s1)))
          (is (false? (:ok s1))))))))


;; +-------------------------------------------------------------+
;; |                    Repeating Patterns.                      |
;; +-------------------------------------------------------------+


(deftest test-0900
  (let [s1 (parse lex-basic/identifier "f")]
    (testing "identifier -- state must not be empty"
      (is (= "f" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0905
  (let [s1 (parse (k/many lex-basic/identifier) "f")]
    (testing "identifier -- many should work with a single-letter id"
      (is (= ["f"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0910
  (let [s1 (parse (lex-basic/comma-sep lex-basic/identifier) "f")]
    (testing "multiple identifier"
      (is (= ["f"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0915
  (let [s1 (parse (k/many (lex-basic/sym \Q)) "QQQ")]
    (testing "multiple symbols"
      (is (= [\Q \Q \Q] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0920
  (let [s1 (parse (k/many (lex-basic/one-of "abc")) "a b c")]
    (testing "many one-of"
      (is (= [\a \b \c] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0925
  (let [s1 (parse (k/many (lex-basic/none-of "abc")) "xyz")]
    (testing "many none-of"
      (is (= [\x \y \z] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0930
  (let [s1 (parse (k/many (lex-basic/token "abc")) "abcabc abc")]
    (testing "many token"
      (is (= ["abc" "abc" "abc"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0935
  (let [s1 (parse (k/many0 (lex-basic/token "abc")) "xxx")]
    (testing "many0, nothing match but empty is cleared"
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= (seq "xxx") (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0940
  (let [s1 (parse (k/many (lex-basic/field " ")) "now is the time")]
    (testing "many field"
      (is (= ["now" "is" "the" "time"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0945
  (let [s1 (parse (k/many (lex-basic/field " ")) "x y")]
    (testing "many token"
      (is (= ["x" "y"] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0950
  (let [s1 (parse (k/many lex-basic/dec-lit) "0")]
    (testing "many dec-lit"
      (is (= [0] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0955
  (let [s1 (parse (k/many lex-basic/oct-lit) "01")]
    (testing "many oct-lit"
      (is (= [1] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0960
  (let [s1 (parse (k/many lex-basic/hex-lit) "0x1")]
    (testing "many hex-lit"
      (is (= [1] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0965
  (let [s1 (parse (k/many lex-basic/float-lit) "1")]
    (testing "many float-lit"
      (is (= [1.0] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))
