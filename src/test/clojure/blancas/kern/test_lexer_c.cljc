;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-c
  (:require [blancas.kern.core :as k :refer [parse >>]]
            [blancas.kern.lexer.c-style :as lex]
            [clojure.test :refer [deftest is testing]]))

;; +-------------------------------------------------------------+
;; |                    Java-style lexers.                       |
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
  (let [s1 (parse (>> lex/trim (k/many1 k/letter)) "  /* comment */ \t\n\t\t ABC")]
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
  (let [s1 (parse (lex/lexeme (k/many1 k/letter)) "ABC /* that's it */ \t\n\t\t")]
    (testing "lexeme - whitespace and comments after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0030
  (let [s1 (parse (lex/lexeme (k/many1 k/letter)) "foo // and the rest is history\nbar")]
    (testing "trim - single-line comment"
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\b \a \r] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0035
  (let [in "foo // variable\n// that's all\n// for now\nbar"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple single-line comments"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0040
  (let [in "foo /* var foo\n  that's all\n for now */\nbar"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0045
  (let [in "foo/********this is a comment**********/"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (empty? (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0050
  (let [in "foo/****** this is a comment*****"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)
        em (k/get-msg-str (:error s1))]
    (testing "lexeme - fails looking for end of comment"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected end of input\nexpecting end of comment"
                :cljs "unexpected end of input\nexpecting end of comment")
             em)))))


(deftest test-0055
  (let [in "foo/******* this is a /* CAN I NEST? */ comment ********/"
        s1 (parse (lex/lexeme (k/many1 k/letter)) in)]
    (testing "lexeme - Won't da nested comment; but this works and stops at 'comment'"
      (is (= (seq "comment ********/") (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0060
  (let [s1 (parse lex/char-lit "'z'")]
    (testing "char-lit"
      (is (= \z (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0065
  (let [s1 (parse lex/char-lit "'\\b'")]
    (testing "char-lit"
      (is (= \backspace (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0070
  (let [s1 (parse lex/char-lit "'\\t'")]
    (testing "char-lit"
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0075
  (let [s1 (parse lex/char-lit "'\\n'")]
    (testing "char-lit"
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0080
  (let [s1 (parse lex/char-lit "'\\f'")]
    (testing "char-lit"
      (is (= \formfeed (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0085
  (let [s1 (parse lex/char-lit "'\\r'")]
    (testing "char-lit"
      (is (= \return (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0090
  (let [s1 (parse lex/char-lit "'\\''")]
    (testing "char-lit"
      (is (= \' (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0095
  (let [s1 (parse lex/char-lit "'\\\"'")]
    (testing "char-lit"
      (is (= \" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0100
  (let [s1 (parse lex/char-lit "'\\\\'")]
    (testing "char-lit"
      (is (= \\ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0105
  (let [s1 (parse lex/char-lit "'\\/'")]
    (testing "char-lit"
      (is (= \/ (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0110
  (let [s1 (parse lex/char-lit "'\\u0041'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0115
  (let [s1 (parse lex/char-lit "'\\101'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0120
  (let [in "'a "
        s1 (parse lex/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails without the closing quote"
      (is (= [\space] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\space\nexpecting end of character literal"
                :cljs "unexpected \" \"\nexpecting end of character literal")
             em)))))


(deftest test-0125
  (let [in "'\\u2'"
        s1 (parse lex/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails: incomplete unicode number"
      (is (= [\'] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\'\nexpecting hexadecimal digit"
                :clsj "unexpected \"'\"\nexpecting hexadecimal digit")
             em)))))


(deftest test-0130
  (let [in "'\\00*"
        s1 (parse lex/char-lit in)
        em (k/get-msg-str (:error s1))]
    (testing "char-lit - fails: octal numbers are assumed; that's acually char \0"
      (is (= [\0 \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\0\nexpecting end of character literal"
                :cljs "unexpected \"0\"\nexpecting end of character literal")
             em)))))


(deftest test-0135
  (let [s1 (parse lex/char-lit "'\\?'")]
    (testing "char-lit"
      (is (= \? (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0140
  (let [s1 (parse lex/char-lit "'\\a'")]
    (testing "char-lit"
      (is (= (char 7) (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0145
  (let [s1 (parse lex/char-lit "'\\v'")]
    (testing "char-lit"
      (is (= (char 11) (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0150
  (let [s1 (parse lex/char-lit "'\\0'")]
    (testing "char-lit"
      (is (= (char 0) (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0155
  (let [s1 (parse lex/char-lit "'\\x41'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0160
  (let [s1 (parse lex/char-lit "'\\U00000041'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0165
  (let [in "\"\\bnow\\tis\\nthe\\ftime\\r\" \t\t|;"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses a simple string literal"
      (is (= [\| \;] (:input s1)))
      (is (= "\bnow\tis\nthe\ftime\r" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0170
  (let [in "\"null-terminated\\0\";"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses a null-terminated string"
      (is (= [\;] (:input s1)))
      (is (= "null-terminated\u0000" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0175
  (let [in "\"\\\\null-terminated\\?\";"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses a backslash and a question mark"
      (is (= [\;] (:input s1)))
      (is (= "\\null-terminated?" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0180
  (let [in "\"\\tnow is \\u0074\\u0068\\u0065 time\" \t\t|;"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses unicode characters"
      (is (= [\| \;] (:input s1)))
      (is (= "\tnow is the time" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0185
  (let [in "\"now is \\164\\150\\145 time\" /* the */|;"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses octal characters"
      (is (= [\| \;] (:input s1)))
      (is (= "now is the time" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0190
  (let [in "\"now is \\x74\\x68\\x65 time\" \t\t|;"
        s1 (parse lex/string-lit in)]
    (testing "string-lit - parses hex characters"
      (is (= [\| \;] (:input s1)))
      (is (= "now is the time" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))
