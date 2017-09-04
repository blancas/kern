;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-core
  (:require [blancas.kern.core :as k :refer [parse value return fail
                                             >>= >> <*>  <?> <|> <+> << <:> <$>
                                             letter digit   satisfy many space new-line* tab
                                             sym* token* token- word* one-of* none-of* eof
                                             many1 optional option skip skip-many skip-many1 sep-by sep-by1 end-by end-by1
                                             sep-end-by sep-end-by1 between alpha-num times dec-num]]
            [blancas.kern.char :as char]
            [clojure.test :refer [deftest is testing]]
            [blancas.kern.char :as char])
  #?(:clj (:require [blancas.kern.core :refer [bind]])
     :cljs (:require-macros [blancas.kern.core :refer [bind]])))

(defn check-pos [line col pos]
  (is (= line (:line pos)))
  (is (= col (:col pos))))

;; +-------------------------------------------------------------+
;; |                       Basic parsers.                        |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse letter "xyz")
        s2 ((return 0) s1)]
    (testing "return"
      (is (= 0 (:value s2)))
      (is (:ok s2))
      (is (nil? (:error s2)))
      (is (= (:input s1) (:input s2)))
      (is (= (:empty s1) (:empty s2)))
      (is (= (:pos s1) (:pos s2)))
      (is (= (:user s1)))) (:user s2)))


(deftest test-0005
  (let [em "the buck stops here"
        s1 (parse letter "xyz")
        s2 ((fail em) s1)]
    (testing "fail"
      (is (nil? (:value s2)))
      (is (false? (:ok s2)))
      (is (:empty s2))
      (is (= (k/make-err-message (:pos s1) em) (:error s2)))
      (is (= (:input s1) (:input s2)))
      (is (= (:pos s1) (:pos s2)))
      (is (= (:user s1)))) (:user s2)))


(deftest test-0010
  (let [s1 (parse letter "xyz")
        s2 ((satisfy char/is-letter) s1)]
    (testing "satisfy - advances one char"
      (is (= [\z] (:input s2)))
      (is (= \y (:value s2)))
      (is (:ok s2))
      (is (false? (:empty s2)))
      (is (= (:user s1) (:user s2)))
      (is (nil? (:error s2)))
      (check-pos 1 3 (:pos s2)))))


(deftest test-0015
  (let [s1 (parse letter "u2")
        s2 ((satisfy char/is-digit) s1)]
    (testing "satisfy - reaches the end of input"
      (is (empty? (:input s2)))
      (is (= \2 (:value s2)))
      (is (:ok s2))
      (is (false? (:empty s2)))
      (is (= (:user s1) (:user s2)))
      (is (nil? (:error s2)))
      (check-pos 1 3 (:pos s2)))))


(deftest test-0020
  (let [s1 (parse letter "u\t")
        s2 ((satisfy #(= \tab %)) s1)]
    (testing "satisfy - advnaces one tab; default 4 positions"
      (is (= \tab (:value s2)))
      (is (:ok s2))
      (is (false? (:empty s2)))
      (is (= (:user s1) (:user s2)))
      (is (nil? (:error s2)))
      (check-pos 1 6 (:pos s2)))))


(deftest test-0025
  (binding [k/*tab-width* 8]
    (let [s1 (parse letter "u\t")
          s2 ((satisfy #(= \tab %)) s1)]
      (testing "satisfy - advances one tab of 8 positions"
        (is (= \tab (:value s2)))
        (is (:ok s2))
        (is (false? (:empty s2)))
        (is (= (:user s1) (:user s2)))
        (is (nil? (:error s2)))
        (check-pos 1 10 (:pos s2))))))


(deftest test-0030
  (let [s1 (parse letter "u\n")
        s2 ((satisfy #(= \newline %)) s1)]
    (testing "satisfy - advances to the next line, first column"
      (is (= \newline (:value s2)))
      (is (:ok s2))
      (is (false? (:empty s2)))
      (is (= (:user s1) (:user s2)))
      (is (nil? (:error s2)))
      (check-pos 2 1 (:pos s2)))))


(deftest test-0035
  (let [em "end of input"
        s1 (parse (many letter) "xyz")
        s2 ((satisfy (fn [_] true)) s1)]
    (testing "satisfy - attempts to read past the end of input"
      (is (empty? (:input s2)))
      (is (nil? (:value s2)))
      (is (false? (:ok s2)))
      (is (:empty s2))
      (is (= (:user s1) (:user s2)))
      (is (= (k/make-err-unexpect (:pos s1) em) (:error s2)))
      (is (= (:pos s1)))) (:pos s2)))


(deftest test-0040
  (let [em #?(:clj "\\2" :cljs "\"2\"")
        s1 (parse letter "u2")
        s2 ((satisfy char/is-letter) s1)]
    (testing "satisfy - the predicate fails"
      (is (= [\2] (:input s2)))
      (is (nil? (:value s2)))
      (is (false? (:ok s2)))
      (is (:empty s2))
      (is (= (:user s1) (:user s2)))
      (is (= (k/make-err-system (:pos s1) em) (:error s2)))
      (is (= (:pos s1)))) (:pos s2)))


;; +-------------------------------------------------------------+
;; |                    Primitive parsers.                       |
;; +-------------------------------------------------------------+


(deftest test-0045
  (let [in "#(f %)"
        s1 (parse k/any-char in)]
    (testing "any-char"
      (is (= (rest in) (:input s1)))
      (is (= \# (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (= (:user s1) (:user s1)))
      (is (nil? (:error s1)))
      (check-pos 1 2 (:pos s1)))))


(deftest test-0050
  (let [in "xyz"
        s1 (parse (>>= k/any-char
                         (fn [a]
                           (>>= k/any-char
                                (fn [b]
                                  (>>= k/any-char
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "any-char - three in a row"
      (is (= (seq in)))) (:value s1)))


(deftest test-0055
  (let [em "end of input"
        s1 (parse k/any-char "")]
    (testing "any-char - fails on end of input"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= (:user s1) (:user s1)))
      (is (= (k/make-err-unexpect (:pos s1) em) (:error s1)))
      (check-pos 1 1 (:pos s1)))))


(deftest test-0060
  (let [in "abc"
        s1 (parse letter in)]
    (testing "letter - parses a single letter"
      (is (= (rest in) (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0065
  (let [in "xyz"
        s1 (parse (>>= letter
                         (fn [a]
                           (>>= letter
                                (fn [b]
                                  (>>= letter
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "letter - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0070
  (let [in "123"
        s1 (parse letter in)]
    (testing "letter - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0075
  (let [in "abc"
        s1 (parse k/lower in)]
    (testing "k/lower - parses a single k/lower-case letter"
      (is (= (rest in) (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0080
  (let [in "xyz"
        s1 (parse (>>= k/lower
                         (fn [a]
                           (>>= k/lower
                                (fn [b]
                                  (>>= k/lower
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "k/lower - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0085
  (let [in "XYZ"
        s1 (parse k/lower in)]
    (testing "k/lower - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0090
  (let [in "ABC"
        s1 (parse k/upper in)]
    (testing "k/upper - parses a single k/upper-case letter"
      (is (= (rest in) (:input s1)))
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0095
  (let [in "XYZ"
        s1 (parse (>>= k/upper
                         (fn [a]
                           (>>= k/upper
                                (fn [b]
                                  (>>= k/upper
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "k/upper - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0100
  (let [in "123"
        s1 (parse k/upper in)]
    (testing "k/upper - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0105
  (let [in " \t\t"
        s1 (parse k/white-space in)]
    (testing "k/white-space - parses a single whitespace character"
      (is (= (rest in) (:input s1)))
      (is (= \space (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0110
  (let [in " \t "
        s1 (parse (>>= k/white-space
                         (fn [a]
                           (>>= k/white-space
                                (fn [b]
                                  (>>= k/white-space
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "k/white-space - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0115
  (let [in "***"
        s1 (parse k/white-space in)]
    (testing "k/white-space - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0120
  (let [in "   "
        s1 (parse space in)]
    (testing "space - parses a single space character"
      (is (= (rest in) (:input s1)))
      (is (= \space (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0125
  (let [in "   "
        s1 (parse (>>= space
                         (fn [a]
                           (>>= space
                                (fn [b]
                                  (>>= space
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "space - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0130
  (let [in "***"
        s1 (parse space in)]
    (testing "space - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0135
  (let [in "\n\t."
        s1 (parse new-line* in)]
    (testing "new-line - parses a single newline character"
      (is (= (rest in) (:input s1)))
      (is (= \newline (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0140
  (let [in "\n\n\n"
        s1 (parse (>>= new-line*
                         (fn [a]
                           (>>= new-line*
                                (fn [b]
                                  (>>= new-line*
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "new-line* - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0145
  (let [in "***"
        s1 (parse new-line* in)]
    (testing "new-line* - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0150
  (let [in "\t|\t|"
        s1 (parse tab in)]
    (testing "tab - parses a single tab character"
      (is (= (rest in) (:input s1)))
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0155
  (let [in "\t\t\t"
        s1 (parse (>>= tab
                         (fn [a]
                           (>>= tab
                                (fn [b]
                                  (>>= tab
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "tab - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0160
  (let [in "***"
        s1 (parse tab in)]
    (testing "tab - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0165
  (let [in "12345"
        s1 (parse digit in)]
    (testing "digit - parses a single digit"
      (is (= (rest in) (:input s1)))
      (is (= \1 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0170
  (let [in "012"
        s1 (parse (>>= digit
                         (fn [a]
                           (>>= digit
                                (fn [b]
                                  (>>= digit
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "digit - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0175
  (let [in "***"
        s1 (parse digit in)]
    (testing "digit - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0180
  (let [in "ABCDEF"
        s1 (parse k/hex-digit in)]
    (testing "hex-digit - parses a single hex digit"
      (is (= (rest in) (:input s1)))
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0185
  (let [in "CAB"
        s1 (parse (>>= k/hex-digit
                         (fn [a]
                           (>>= k/hex-digit
                                (fn [b]
                                  (>>= k/hex-digit
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "hex-digit - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0190
  (let [in "***"
        s1 (parse digit in)]
    (testing "hex-digit - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0195
  (let [in "12345"
        s1 (parse k/oct-digit in)]
    (testing "oct-digit - parses a single octal digit"
      (is (= (rest in) (:input s1)))
      (is (= \1 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0200
  (let [in "567"
        s1 (parse (>>= k/oct-digit
                         (fn [a]
                           (>>= k/oct-digit
                                (fn [b]
                                  (>>= k/oct-digit
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "oct-digit - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0205
  (let [in "***"
        s1 (parse digit in)]
    (testing "oct-digit - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0210
  (let [in "a0b1"
        s1 (parse k/alpha-num in)]
    (testing "alpha-num - parses a single alpha-numeric character"
      (is (= (rest in) (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0215
  (let [in "a1b"
        s1 (parse (>>= k/alpha-num
                         (fn [a]
                           (>>= k/alpha-num
                                (fn [b]
                                  (>>= k/alpha-num
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "alpha-num - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0220
  (let [in "+*&"
        s1 (parse k/alpha-num in)]
    (testing "alpha-num - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0225
  (let [in "X()"
        s1 (parse (sym* \X) in)]
    (testing "sym* - parses a single X"
      (is (= (rest in) (:input s1)))
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0230
  (let [in "p\t;"
        s1 (parse (>>= (sym* \p)
                         (fn [a]
                           (>>= (sym* \tab)
                                (fn [b]
                                  (>>= (sym* \;)
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "sym* - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0235
  (let [in "***"
        s1 (parse (sym* \X) in)]
    (testing "sym* - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0235-05
  (let [in "x()"
        s1 (parse (k/sym- \X) in)]
    (testing "sym- - parses a single x"
      (is (= (rest in) (:input s1)))
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0235-10
  (let [in "X()"
        s1 (parse (k/sym- \X) in)]
    (testing "sym- - parses a single X"
      (is (= (rest in) (:input s1)))
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0235-15
  (let [in "A()"
        s1 (parse (k/sym- \X) in)
        em (k/get-msg-str (:error s1))]
    (testing "sym- - parses a single X"
      (is (= [\A \( \)] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= em #?(:clj "unexpected \\A\nexpecting \\X"
                   :cljs "unexpected \"A\"\nexpecting \"X\""))))))


(deftest test-0240
  (let [in "program foo()"
        s1 (parse (token* "program") in)]
    (testing "token* - parses a specific word"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0245
  (let [in "foo(bar)baz"
        s1 (parse (>>= (token* "foo")
                         (fn [a]
                           (>>= (token* "(bar)")
                                (fn [b]
                                  (>>= (token* "baz")
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "token* - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= ["foo" "(bar)" "baz"]))) (:value s1)))


(deftest test-0250
  (let [in "goat"
        s1 (parse (token* "goal") in)
        em (k/get-msg-str (:error s1))]
    (testing "token* - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= em "unexpected goat\nexpecting goal")))))


(deftest test-0250-05
  (let [in "function foo()"
        s1 (parse (token* "function" "procedure") in)]
    (testing "token* - parses one of multiple word choices"
      (is (= (drop (count "function") in) (:input s1)))
      (is (= "function" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-10
  (let [in "procedure foo()"
        s1 (parse (token* "function" "procedure") in)]
    (testing "token* - parses one of multiple word choices"
      (is (= (drop (count "procedure") in) (:input s1)))
      (is (= "procedure" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-15
  (let [in "program foo()"
        s1 (parse (token- "PROGRAM") in)]
    (testing "token- - parses a specific word; non case-sensetive"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "PROGRAM" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-20
  (let [in "Program foo()"
        s1 (parse (token- "PROGRAM") in)]
    (testing "token- - parses a specific word; non case-sensetive"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "PROGRAM" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-25
  (let [in "PROGRAM foo()"
        s1 (parse (token- "PROGRAM") in)]
    (testing "token- - parses a specific word; non case-sensetive"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "PROGRAM" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-30
  (let [in "goat"
        s1 (parse (token- "goal") in)
        em (k/get-msg-str (:error s1))]
    (testing "token- - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (= em "unexpected goat\nexpecting goal"))))


(deftest test-0250-35
  (let [in "FUNction foo()"
        s1 (parse (token- "function" "procedure") in)]
    (testing "token- - parses one of multiple word choices"
      (is (= (drop (count "function") in) (:input s1)))
      (is (= "function" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-40
  (let [in "PROCedure foo()"
        s1 (parse (token- "function" "procedure") in)]
    (testing "token- - parses one of multiple word choices"
      (is (= (drop (count "procedure") in) (:input s1)))
      (is (= "procedure" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-45
  (let [in "program foo()"
        s1 (parse (word* letter "program") in)]
    (testing "word* - parses a specific, delimited word"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-50
  (let [in "else{}"
        s1 (parse (word* letter "else") in)]
    (testing "word* - parses a specific, delimited word"
      (is (= [\{ \}] (:input s1)))
      (is (= "else" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-55
  (let [in "procedure"
        s1 (parse (word* letter "proc") in)
        em (k/get-msg-str (:error s1))]
    (testing "word* - fails because is not delimited"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= em "unexpected e\nexpecting end of proc")))))


(deftest test-0250-60
  (let [in "otherwise{}"
        s1 (parse (word* letter "else" "otherwise") in)]
    (testing "word* - parses a specific, delimited word"
      (is (= [\{ \}] (:input s1)))
      (is (= "otherwise" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-65
  (let [in "subroutine"
        s1 (parse (word* letter "proc" "func" "method") in)
        em (k/get-msg-str (:error s1))]
    (testing "word* - fails with incorrect input"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= em "unexpected subr\nunexpected subrou\nexpecting proc, func or method")))))


(deftest test-0250-70
  (let [in "PROGRAM foo()"
        s1 (parse (k/word- letter "program") in)]
    (testing "word- - parses a specific, delimited word; not case-senstive"
      (is (= (drop (count "program") in) (:input s1)))
      (is (= "program" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-75
  (let [in "Else{}"
        s1 (parse (k/word- letter "else") in)]
    (testing "word- - parses a specific, delimited word; not case-senstive"
      (is (= [\{ \}] (:input s1)))
      (is (= "else" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-80
  (let [in "ProcEdure"
        s1 (parse (k/word- letter "proc") in)
        em (k/get-msg-str (:error s1))]
    (testing "word- - fails because is not delimited"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected E\nexpecting end of proc" em)))))


(deftest test-0250-85
  (let [in "OtherWise{}"
        s1 (parse (k/word- letter "else" "otherwise") in)]
    (testing "word- - parses a specific, delimited word; not case-senstive"
      (is (= [\{ \}] (:input s1)))
      (is (= "otherwise" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0250-90
  (let [in "SUBroutine"
        s1 (parse (k/word- letter "proc" "func" "method") in)
        em (k/get-msg-str (:error s1))]
    (testing "word- - fails with incorrect input"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= "unexpected SUBr\nunexpected SUBrou\nexpecting proc, func or method" em)))))


(deftest test-0255
  (let [in "* 2"
        s1 (parse (one-of* "+-*/^") in)]
    (testing "one-of* - parses one of the supplied characters"
      (is (= (rest in) (:input s1)))
      (is (= \* (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0260
  (let [in "*-+"
        op "+-*/"
        s1 (parse (>>= (one-of* op)
                         (fn [a]
                           (>>= (one-of* op)
                                (fn [b]
                                  (>>= (one-of* op)
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "one-of* - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0265
  (let [in "abc"
        s1 (parse (one-of* "+-*/") in)]
    (testing "one-of* - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0270
  (let [in ": 2"
        s1 (parse (none-of* "+-*/^") in)]
    (testing "none-of* - parses a character not supplied"
      (is (= (rest in) (:input s1)))
      (is (= \: (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0275
  (let [in "^&%"
        op "+-*/"
        s1 (parse (>>= (none-of* op)
                         (fn [a]
                           (>>= (none-of* op)
                                (fn [b]
                                  (>>= (none-of* op)
                                       (fn [c]
                                         (return [a b c])))))))
                    in)]
    (testing "none-of* - three in a row until end of input"
      (is (empty? (:input s1)))
      (is (= (seq in)))) (:value s1)))


(deftest test-0280
  (let [in "$foo"
        s1 (parse (none-of* "!@#$%^*()") in)]
    (testing "none-of* - fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0280-05
  (let [in ""
        s1 (parse eof in)]
    (testing "eof - parses an empty string"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1)))))


(deftest test-0280-10
  (let [in "END."
        s1 (parse (>> (token* "END.") eof) in)]
    (testing "eof - verifies that input ends"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-15
  (let [in "END.// the end"
        s1 (parse (>> (token* "END.") eof) in)
        em (k/get-msg-str (:error s1))]
    (testing "eof - verifies that input ends"
      (is (= (seq "// the end") (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "unexpected /\nexpecting end of input" em)))))


(deftest test-0280-20
  (let [in "12\n"
        s1 (parse (<*> digit digit new-line*) in)]
    (testing "new-line* - a new line after two digits"
      (is (empty? (:input s1)))
      (is (= [\1 \2 \newline] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-25
  (let [in "AB\r\nCD\r\n"
        s1 (parse (many1 (<< (many1 k/upper) new-line*)) in)]
    (testing "new-line* - pairs of letters separated by a new line"
      (is (empty? (:input s1)))
      (is (= [[\A \B] [\C \D]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-30
  (let [in "12345 "
        s1 (parse (<< (many1 digit) new-line*) in)
        em (k/get-msg-str (:error s1))]
    (testing "new-line* - the line doesn't end with a new line"
      (is (= [\space] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\space\nexpecting new line"
                :cljs "unexpected \" \"\nexpecting new line")
             em)))))


(deftest test-0280-35
  (let [in "   \t \t \n \t *"
        s1 (parse (k/skip-ws (sym* \*)) in)]
    (testing "skip-ws - skips whitespaces before parsing a star"
      (is (empty? (:input s1)))
      (is (= \* (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-40
  (let [in "*"
        s1 (parse (k/skip-ws (sym* \*)) in)]
    (testing "skip-ws - nothing to skip before parsing a star"
      (is (empty? (:input s1)))
      (is (= \* (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-45
  (let [in "Now is the time...  right... now."
        s1 (parse (k/field* "!") in)]
    (testing "field* - reads the whole string"
      (is (empty? (:input s1)))
      (is (= in (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-50
  (let [in "Now is the time; right... now."
        s1 (parse (k/field* ";") in)]
    (testing "field* - reads the field delimited by a semicolon"
      (is (= (seq "; right... now.") (:input s1)))
      (is (= "Now is the time" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-55
  (let [in "Now-is-the-time"
        s1 (parse (k/split-on "-") in)]
    (testing "field - breaks the string into the words"
      (is (= ["Now" "is" "the" "time"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-60
  (let [in "Software,Tooling,495.95,0.00,,15,,xyz"
        s1 (parse (k/split-on ",") in)]
    (testing "field - breaks the string into fields; some are empty"
      (is (= ["Software" "Tooling" "495.95" "0.00" "15" "xyz"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-65
  (let [in "Now is the time.   Or, is it?     Yes! yes! that's it."
        s1 (parse (k/split-on " ,?!.") in)]
    (testing "field - collects all words; skips the given punctuation"
      (is (= ["Now" "is" "the" "time" "Or" "is" "it" "Yes" "yes" "that's" "it"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0280-70
  (testing "mark parses a punctuation mark."
    (is (= \! (value k/mark "!")))
    (is (= \@ (value k/mark "@")))
    (is (= \* (value k/mark "*")))
    (is (= \: (value k/mark ":")))
    (is (= \/ (value k/mark "/")))
    (is (= \. (value k/mark ".")))))


;; +-------------------------------------------------------------+
;; |                     Parser combinators.                     |
;; +-------------------------------------------------------------+


(deftest test-0285
  (let [s1 (parse letter "1")
        em (-> s1 :error :msgs first :text force)]
    (testing "first message in the msgs list"
      (is (= "letter" em)))))


(deftest test-0290
  (let [s1 (parse letter "1")
        em (k/get-msg-str (:error s1))]
    (testing "verify error messages"
      (is (= #?(:clj "unexpected \\1\nexpecting letter"
                :cljs "unexpected \"1\"\nexpecting letter")
             em)))))


(deftest test-0295
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "01")
        em (-> s1 :error :msgs first :text force)]
    (testing "<?> - does not add message when input is consumed"
      (is (not= "digit,letter" em)))))


(deftest test-0300
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "01")
        em (k/get-msg-str (:error s1))]
    (testing "<?> - verifies error messages in <*>"
      (is (= #?(:clj "unexpected \\1\nexpecting letter"
                :cljs "unexpected \"1\"\nexpecting letter")
             em)))))


(deftest test-0305
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "0")
        em (k/get-msg-str (:error s1))]
    (testing "<?> - verifies error messages in <*>"
      (is (= "unexpected end of input\nexpecting letter" em)))))


(deftest test-0310
  (let [s1 (parse (<|> digit letter) "*")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - verifies error messages"
      (is (= #?(:clj "unexpected \\*\nexpecting digit or letter"
                :cljs "unexpected \"*\"\nexpecting digit or letter")
             em)))))


(deftest test-0315
  (let [s1 (parse (<|> (sym* \x) (<|> letter digit)) "*")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - verifies error messages with 3 choices"
      (is (= #?(:clj "unexpected \\*\nexpecting \\x, letter or digit"
                :cljs "unexpected \"*\"\nexpecting \"x\", letter or digit")
             em)))))


(deftest test-0320
  (let [s1 (parse (<|> (<|> k/white-space (sym* \x)) (<|> letter digit)) "*")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - verifies error messages with 4 choices"
      (is (= #?(:clj "unexpected \\*\nexpecting whitespace, \\x, letter or digit"
                :cljs "unexpected \"*\"\nexpecting whitespace, \"x\", letter or digit")
             em)))))


(deftest test-0320-05
  (let [s1 (parse (k/expect (<+> letter digit) "number two") "U2")]
    (testing "expect - parser succeeds"
      (is (= "U2" (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0320-10
  (let [s1 (parse (k/expect (<+> letter digit) "number two") "UX")
        em (k/get-msg-str (:error s1))]
    (testing "expect - parser fails consuming input"
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\X\nexpecting number two"
                :cljs "unexpected \"X\"\nexpecting number two")
             em)))))


(deftest test-0320-15
  (let [s1 (parse (k/expect (<+> letter digit) "number two") "007")
        em (k/get-msg-str (:error s1))]
    (testing "expect - parser fails without consuming any input"
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= [\0 \0 \7] (:input s1)))
      (is (= #?(:clj "unexpected \\0\nexpecting number two"
                :cljs "unexpected \"0\"\nexpecting number two")
             em)))))


(deftest test-0325
  (let [s1 (parse (<|> letter digit) "U2")]
    (testing "<|> - the first parser succeeds"
      (is (= \U (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\2] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0330
  (let [s1 (parse (<|> digit letter) "XYZ")]
    (testing "<|> - the second parser succeeds"
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (= [\Y \Z] (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0335
  (let [s1 (parse (<|> (>> letter digit) letter) "XYZ")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - the first parse fails consuming input"
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (= [\Y \Z] (:input s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\Y\nexpecting digit"
                :cljs "unexpected \"Y\"\nexpecting digit")
             em)))))


(deftest test-0340
  (let [s1 (parse (<|> k/white-space letter digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - verifies error messages with 3 choices"
      (is (= #?(:clj "unexpected \\*\nexpecting whitespace, letter or digit"
                :cljs "unexpected \"*\"\nexpecting whitespace, letter or digit")
             em)))))


(deftest test-0345
  (let [s1 (parse (<|> k/white-space (sym* \x) letter digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "<|> - verifies error messages with 4 choices"
      (is (= #?(:clj "unexpected \\*\nexpecting whitespace, \\x, letter or digit"
                :cljs "unexpected \"*\"\nexpecting whitespace, \"x\", letter or digit")
             em)))))


(deftest test-0350
  (let [s1 (parse (<|> k/white-space (sym* \x) letter digit) "\t")]
    (testing "<|> - the first of 4 parser succeeds"
      (is (= \tab (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0355
  (let [s1 (parse (<|> k/white-space (sym* \x) letter digit) "x")]
    (testing "<|> - the second of 4 parser succeeds"
      (is (= \x (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0360
  (let [s1 (parse (<|> k/white-space (sym* \x) letter digit) "z")]
    (testing "<|> - the third of 4 parser succeeds"
      (is (= \z (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0365
  (let [s1 (parse (<|> k/white-space (sym* \x) letter digit) "0")]
    (testing "<|> - the fourth parser succeeds"
      (is (= \0 (:value s1)))
      (is (:ok s1))
      (is (nil? (:error s1)))
      (is (empty? (:input s1)))
      (is (false? (:empty s1))))))


(deftest test-0370
  (let [p1 (>>= letter (fn [x] (return (char/upper-case x))))
        s1 (parse p1 "xyz")]
    (testing ">>= - advances one char"
      (is (= [\y \z] (:input s1)))
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1)))
      (check-pos 1 2 (:pos s1)))))


(deftest test-0375
  (let [p1 (>>= digit
                (fn [x] (>>= digit
                             (fn [y] (return #?(:clj (Integer/parseInt (str x y))
                                                :cljs (js/parseInt (str x y))))))))
        s1 (parse p1 "50113")]
    (testing ">>= - advances two chars"
      (is (= [\1 \1 \3] (:input s1)))
      (is (= 50 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1)))
      (check-pos 1 3 (:pos s1)))))


(deftest test-0380
  (let [in "012345"
        p1 (>>= letter (fn [x] (return (int x))))
        s1 (parse p1 in)
        em (k/get-msg-str (:error s1))]
    (testing ">>= - the first parser fails"
      (is (= (seq in) (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (nil? (:user s1)))
      (check-pos 1 1 (:pos s1))
      (is (= #?(:clj "unexpected \\0\nexpecting letter"
                :cljs "unexpected \"0\"\nexpecting letter")
             em)))))


(deftest test-0385
  (let [p1 (>>= letter (fn [_] digit))
        s1 (parse p1 "xyz")
        em (k/get-msg-str (:error s1))]
    (testing ">>= - the second parser fails"
      (is (= [\y \z] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (check-pos 1 2 (:pos s1))
      (is (= #?(:clj "unexpected \\y\nexpecting digit"
                :cljs "unexpected \"y\"\nexpecting digit")
             em)))))


(deftest test-0385-05
  (let [p1 (bind [x letter] (return (char/upper-case x)))
        s1 (parse p1 "xyz")]

    (testing "bind - advances one char"
      (is (= [\y \z] (:input s1)))
      (is (= \X (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1)))
      (check-pos 1 2 (:pos s1)))))


(deftest test-0385-10
  (let [p1 (bind [x digit y digit]
                 (return #?(:clj (Integer/parseInt (str x y))
                            :cljs (js/parseInt (str x y)))))
        s1 (parse p1 "50113")]
    (testing "bind - advances two chars"
      (is (= [\1 \1 \3] (:input s1)))
      (is (= 50 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1)))
      (check-pos 1 3 (:pos s1)))))


(deftest test-0385-15
  (let [p1 (bind [_ (sym* \()
                  s (<+> (many1 digit))
                  _ (sym* \))]
                 (return (* #?(:clj (Integer/parseInt s) :cljs (js/parseInt s))
                            -1)))
        s1 (parse p1 "(50113)")]
    (testing "bind - reads a negative number in parens, as in accounting"
      (is (empty? (:input s1)))
      (is (= -50113 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1))))))


(deftest test-0385-20
  (let [p1 (bind [x letter]
                 (if (= x \x)
                   (bind [y (sym* \Y)
                          z (sym* \Z)] (return "first"))
                   (bind [n (many1 digit)] (return 5005))))
        s1 (parse p1 "xYZ")]
    (testing "bind - uses nested bind inside the first function body"
      (is (empty? (:input s1)))
      (is (= "first" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1))))))


(deftest test-0385-25
  (let [p1 (bind [x letter]
                 (if (= x \x)
                   (bind [y (sym* \Y)
                          z (sym* \Z)] (return "first"))
                   (bind [n (many1 digit)] (return 666))))
        s1 (parse p1 "A10002450")]
    (testing "bind - uses nested bind inside the first function body"
      (is (empty? (:input s1)))
      (is (= 666 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1))))))


(deftest test-0385-30
  (let [p1 (<|> (bind [x (many1 digit)] (return true))
                (bind [x (many1 letter)] (return false)))
        s1 (parse p1 "FALSE")]
    (testing "bind - the first bind fails, the second succeeds"
      (is (empty? (:input s1)))
      (is (false? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:user s1)))
      (is (nil? (:error s1))))))


(deftest test-0390
  (let [p1 (>> (sym* \+) digit)
        s1 (parse p1 "+1")]
    (testing ">> - consumes two chars, keeps the second"
      (is (empty? (:input s1)))
      (is (= \1 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0395
  (let [p1 (>> (sym* \+) digit)
        s1 (parse p1 "01")]
    (testing ">> - the first parser fails"
      (is (= [\0 \1] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0400
  (let [p1 (>> (sym* \+) digit)
        s1 (parse p1 "+A")]
    (testing ">> - the second parser fails"
      (is (= [\A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0405
  (let [p1 (>> digit digit letter)
        s1 (parse p1 "01A")]
    (testing ">> - consumes three chars, keeps the last"
      (is (empty? (:input s1)))
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0410
  (let [p1 (>> digit digit digit letter)
        s1 (parse p1 "012A")]
    (testing ">> - consumes four chars, keeps the last"
      (is (empty? (:input s1)))
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0415
  (let [p1 (>> digit digit digit letter)
        s1 (parse p1 "A")]
    (testing ">> - the first fails"
      (is (= [\A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0420
  (let [p1 (>> digit digit digit letter)
        s1 (parse p1 "01A")]
    (testing ">> - the third fails"
      (is (= [\A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0425
  (let [p1 (<< letter (sym* \;))
        s1 (parse p1 "a;")]
    (testing "<< - consumes two chars, keeps the first"
      (is (empty? (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0430
  (let [p1 (<< letter (sym* \;))
        s1 (parse p1 "0;")]
    (testing "<< - the first parser fails"
      (is (= [\0 \;] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0435
  (let [p1 (<< letter (sym* \;))
        s1 (parse p1 "A*")]
    (testing "<< - the second parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0440
  (let [p1 (<< k/any-char digit digit)
        s1 (parse p1 "+01")]
    (testing "<< - consumes three chars, keeps the first"
      (is (empty? (:input s1)))
      (is (= \+ (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0445
  (let [p1 (<< k/any-char digit digit digit digit)
        s1 (parse p1 "+0123")]
    (testing "<< - consumes five chars, keeps the first"
      (is (empty? (:input s1)))
      (is (= \+ (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0450
  (let [p1 (<< k/any-char digit digit digit digit)
        s1 (parse p1 "+01*")]
    (testing "<< - the fourth parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0455
  (let [s1 (parse (<$> count (many k/any-char)) "abcdef+01234*")]
    (testing "<$> - counts the length of the input"
      (is (empty? (:input s1)))
      (is (= 13 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0460
  (let [s1 (parse (<$> #(- (int %) (int \0)) digit) "9")]
    (testing "<$> - converts a char digit into an int"
      (is (empty? (:input s1)))
      (is (= 9 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0465
  (let [s1 (parse (<$> #(- (int %) (int \0)) digit) "A")]
    (testing "<$> - fails and the function is not applied"
      (is (= [\A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0469-a
  (let [s1 (parse (<*> digit) "9")]
    (testing "<*> - collects from one parser"
      (is (empty? (:input s1)))
      (is (= [\9] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0469-b
  (let [s1 (parse (<*> letter) "U2")]
    (testing "<*> - collects from one parser"
      (is (= [\2] (:input s1)))
      (is (= [\U] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0470
  (let [s1 (parse (<*> (sym* \-) digit) "-1")]
    (testing "<*> - collects from two parsers"
      (is (empty? (:input s1)))
      (is (= [\- \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0475
  (let [s1 (parse (<*> (sym* \-) digit (sym* \;)) "-1;")]
    (testing "<*> - collects from three parsers"
      (is (empty? (:input s1)))
      (is (= [\- \1 \;] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0480
  (let [p1 (<*> letter (>> (sym* \|) letter) (>> (sym* \|) digit))
        s1 (parse p1 "X|Y|9")]
    (testing "<*> - collects from filtering parsers"
      (is (empty? (:input s1)))
      (is (= [\X \Y \9] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0485
  (let [in "ABC012"
        p1 (<*> (<*> letter letter letter) (<*> digit digit digit))
        s1 (parse p1 in)]
    (testing "<*> - collects from compound parsers"
      (is (empty? (:input s1)))
      (is (= '((\A \B \C) (\0 \1 \2)) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0485-05
  (let [s1 (parse (<*> letter digit) "u2")]
    (testing "<*> - collects results in a vector"
      (is (empty? (:input s1)))
      (is (vector? (:value s1)))
      (is (= [\u \2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0490
  (let [s1 (parse (<*> letter digit) "*")]
    (testing "<*> - the first parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0495
  (let [s1 (parse (<*> letter digit) "A*")]
    (testing "<*> - the second parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0500
  (let [s1 (parse (<*> letter tab digit) "A\t*")]
    (testing "<*> - the third parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0505
  (let [p1 (<*> letter tab tab tab tab tab (sym* \x))
        s1 (parse p1 "A\t\t\t\t\t*")]
    (testing "<*> - the seventh parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-0510
  (let [s1 (parse (<:> k/lower) "a")]
    (testing "<:> - parses an item; consumes all input"
      (is (empty? (:input s1)))
      (is (= \a (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0515
  (let [s1 (parse (<:> (<$> (partial apply str) (<*> (token* "end") space))) "end ")]
    (testing "<:> - parses nested items; consumes all input"
      (is (empty? (:input s1)))
      (is (= "end " (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0520
  (let [in "a1b3c4d5e7f8"
        p1 (<*> letter digit)
        s1 (parse (<:> (many p1)) in)]
    (testing "<:> - parses six pairs; consumes all input"
      (is (empty? (:input s1)))
      (is (= '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8)) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0525
  (let [s1 (parse (<:> k/lower) "*&!")]
    (testing "<:> - fails with parsers consuming no input; consumes no input"
      (is (= [\* \& \!] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-0530
  (let [s1 (parse (<:> (<*> k/upper k/lower k/upper)) "Mi*")
        em (k/get-msg-str (:error s1))]
    (testing "<:> - fails with parsers consuming input; consumes no input"
      (is (= [\M \i \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting uppercase letter"
                :cljs "unexpected \"*\"\nexpecting uppercase letter")
             em)))))


(deftest test-0535
  (let [s1 (parse (<|> (<:> (>> digit letter)) digit) "1*")
        em (k/get-msg-str (:error s1))]
    (testing "<:> - verifies that it allows <|> to test the next choice"
      (is (= [\*] (:input s1)))
      (is (= \1 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0540
  (let [s1 (parse (<|> (<:> letter) digit) "***")
        em (k/get-msg-str (:error s1))]
    (testing "<:> - verifies that it carries over the error msg"
      (is (= [\* \* \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting letter or digit"
                :cljs "unexpected \"*\"\nexpecting letter or digit")
             em)))))


(deftest test-0545
  (let [s1 (parse (many k/lower) "*")]
    (testing "many - parses zero items"
      (is (= [\*] (:input s1)))
      (is (empty? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0550
  (let [s1 (parse (many k/lower) "a*")]
    (testing "many - parses one item"
      (is (= [\*] (:input s1)))
      (is (= [\a] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0555
  (let [s1 (parse (many (optional letter)) "ABCDEGFHIJK*")]
    (testing "many - skips optional items; consumes input though value is empty"
      (is (= [\*] (:input s1)))
      (is (= (seq "ABCDEGFHIJK") (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0560
  (let [in "a1b3c4d5e7f8"
        p1 (<*> letter digit)
        s1 (parse (many p1) in)]
    (testing "many - parses six compound items"
      (is (empty? (:input s1)))
      (is (= '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8)) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0560-05
  (let [s1 (parse (many k/lower) "*")]
    (testing "many - collects the result in a vector; parses zero items"
      (is (= [\*] (:input s1)))
      (is (empty? (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0560-10
  (let [s1 (parse (many k/lower) "a*")]
    (testing "many - collects the result in a vector; parses one item"
      (is (= [\*] (:input s1)))
      (is (= [\a] (:value s1)))

      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0565
  (let [s1 (parse (<|> (many k/lower) (sym* \*)) "*")]
    (testing "many - consumes no input and succeeds; <|> returns its value"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0570
  (let [in "a1b3c4d5ee"
        p1 (<*> letter digit)
        s1 (parse (many p1) in)
        em (k/get-msg-str (:error s1))]
    (testing "many - parses four compound items, then fails in the next compound item"
      (is (= [\e] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\e\nexpecting digit"
                :cljs "unexpected \"e\"\nexpecting digit")
             em)))))


(deftest test-0575
  (let [s1 (parse (many1 k/lower) "*")
        em (k/get-msg-str (:error s1))]
    (testing "many1 - fails with zero items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting lowercase letter"
                :cljs "unexpected \"*\"\nexpecting lowercase letter")
             em)))))


(deftest test-0580
  (let [s1 (parse (many1 k/lower) "a*")]
    (testing "many1 - parses one item"
      (is (= [\*] (:input s1)))
      (is (= [\a] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0580-05
  (let [s1 (parse (many1 k/lower) "a*")]
    (testing "many1 - collects the result in a vector; parses one item"
      (is (= [\*] (:input s1)))
      (is (= [\a] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0585
  (let [in "a1b3c4d5e7f8"
        p1 (<*> letter digit)
        s1 (parse (many1 p1) in)]
    (testing "many1 - parses six compound items"
      (is (empty? (:input s1)))
      (is (= '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8)) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0590
  (let [s1 (parse (<|> (many1 k/lower) (sym* \*)) "w*")]
    (testing "many1 - consumes input; <|> returns its value"
      (is (= [\*] (:input s1)))
      (is (= [\w] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0595
  (let [s1 (parse (<|> (many1 digit) k/upper) "*")
        em (k/get-msg-str (:error s1))]
    (testing "many1 - fails; passes on empty, errors to <|>"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit or uppercase letter"
                :cljs "unexpected \"*\"\nexpecting digit or uppercase letter")
             em)))))


(deftest test-0600
  (let [s1 (parse (<|> (many1 digit) k/upper) "A")]
    (testing "many1 - fails; passes on empty, are cleared in <|>"
      (is (empty? (:input s1)))
      (is (= \A (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-0605
  (let [in "a1b3c4d5ee"
        p1 (<*> letter digit)
        s1 (parse (many1 p1) in)
        em (k/get-msg-str (:error s1))]
    (testing "many1 - parses four compound items, then fails in the next compound item"
      (is (= [\e] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\e\nexpecting digit"
                :cljs "unexpected \"e\"\nexpecting digit")
             em)))))


(deftest test-0610
  (let [s1 (parse (optional (<*> k/upper digit)) "U2*")]
    (testing "optional - parses an optional item"
      (is (= [\*] (:input s1)))
      (is (= [\U \2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0615
  (let [s1 (parse (optional (<*> k/upper digit)) "u2*")]
    (testing "optional - fails consuming no input"
      (is (= [\u \2 \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0620
  (let [s1 (parse (optional (<*> k/upper digit)) "UP*")
        em (k/get-msg-str (:error s1))]
    (testing "optional - fails consuming input"
      (is (= [\P \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\P\nexpecting digit"
                :cljs "unexpected \"P\"\nexpecting digit")
             em)))))


(deftest test-0625
  (let [p1 (<$> (partial apply str) (<*> (optional k/upper) (sym* \*)))
        s1 (parse p1 "U*")]
    (testing "optional - skips the optional char"
      (is (empty? (:input s1)))
      (is (= "U*" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0630
  (let [s1 (parse (option "XY" (<*> k/upper digit)) "U2*")]
    (testing "option - parses an item"
      (is (= [\*] (:input s1)))
      (is (= [\U \2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0635
  (let [s1 (parse (option "XY" (<*> k/upper digit)) "u2*")]
    (testing "option - fails without consuming input; produces optional value"
      (is (= [\u \2 \*] (:input s1)))
      (is (= "XY" (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0640
  (let [s1 (parse (option "XY" (<*> k/upper digit)) "UP*")
        em (k/get-msg-str (:error s1))]
    (testing "option - fails consuming input"
      (is (= [\P \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\P\nexpecting digit"
                :cljs "unexpected \"P\"\nexpecting digit")
             em)))))


(deftest test-0640-05
  (let [s1 (parse (skip (sym* \*)) "*")]
    (testing "skip - skips a star"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0640-10
  (let [s1 (parse (skip letter digit) "U2")]
    (testing "skip - skips a letter and a digit"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0640-15
  (let [s1 (parse (skip (sym* \*) letter digit (sym* \*)) "*U2*")]
    (testing "skip - skips a star, a letter, a digit, and a star"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0645
  (let [s1 (parse (skip-many letter) "*")]
    (testing "skip-many - skips zero letters"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0650
  (let [s1 (parse (skip-many (<*> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "skip-many - skips zero compound items; <*> fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0655
  (let [s1 (parse (skip-many (<*> digit k/lower)) "0x*")]
    (testing "skip-many - skips one compound item"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0660
  (let [s1 (parse (skip-many letter) "abcdefghijk*")]
    (testing "skip-many - skips letters"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0665
  (let [s1 (parse (skip-many (<*> digit k/lower)) "0x1y2z*")]
    (testing "skip-many - skips three compound items; consumes no more input"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0670
  (let [s1 (parse (>> (skip-many (optional digit)) (sym* \*)) "0123456789*")]
    (testing "skip-many - skips optional items; then consumes more input"
      (is (empty? (:input s1)))
      (is (= \* (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0675
  (let [s1 (parse (skip-many1 letter) "*")
        em (k/get-msg-str (:error s1))]
    (testing "skip-many1 - fails with zero letters"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting letter"
                :cljs "unexpected \"*\"\nexpecting letter")
             em)))))


(deftest test-0680
  (let [s1 (parse (skip-many1 (<*> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "skip-many1 - skips zero compound items; <*> fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit") em)))))


(deftest test-0685
  (let [s1 (parse (skip-many1 (<*> digit k/lower)) "0x*")]
    (testing "skip-many1 - skips one compound item"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0690
  (let [s1 (parse (skip-many1 letter) "abcdefghijk*")]
    (testing "skip-many1 - skips letters"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0695
  (let [s1 (parse (skip-many1 (<*> digit k/lower)) "0x1y2z*")]
    (testing "skip-many1 - skips three compound items; consumes no more input"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0700
  (let [s1 (parse (>> (skip-many1 (optional digit)) (sym* \*)) "0123456789*")]
    (testing "skip-many1 - skips optional items; then consumes more input"
      (is (empty? (:input s1)))
      (is (= \* (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0705
  (let [s1 (parse (sep-by (sym* \,) digit) "*")]
    (testing "sep-by - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0710
  (let [s1 (parse (sep-by (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0715
  (let [s1 (parse (sep-by (sym* \,) digit) "0*")]
    (testing "sep-by - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0720
  (let [s1 (parse (sep-by (sym* \,) (<*> k/upper digit)) "U2*")]
    (testing "sep-by - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0725
  (let [s1 (parse (sep-by (sym* \,) (>> letter digit)) "U2,*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter"
                :cljs "unexpected \"*\"\nexpecting letter")
             em)))))


(deftest test-0730
  (let [s1 (parse (sep-by (sym* \,) digit) "0,1*")]
    (testing "sep-by - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0730-05
  (let [s1 (parse (sep-by (sym* \,) digit) "0,1*")]
    (testing "sep-by - collects the result in a vector; two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0735
  (let [s1 (parse (sep-by (sym* \,) (>> (sym* \+) digit)) "+0,+1*")]
    (testing "sep-by - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0740
  (let [p1 (sep-by (many k/white-space) (many letter))
        s1 (parse p1 "one two \t\t  three")]
    (testing "sep-by - three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "three")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0745
  (let [s1 (parse (sep-by1 (sym* \,) digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by1 - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0750
  (let [s1 (parse (sep-by1 (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by1 - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0755
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0*")]
    (testing "sep-by1 - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0760
  (let [s1 (parse (sep-by1 (sym* \,) (<*> k/upper digit)) "U2*")]
    (testing "sep-by1 - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0765
  (let [s1 (parse (sep-by1 (sym* \,) (>> letter digit)) "U2,*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by1 - there is only one item and the separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter"
                :cljs "unexpected \"*\"\nexpecting letter")
             em)))))


(deftest test-0770
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0,1*")]
    (testing "sep-by1 - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0770-05
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0,1*")]
    (testing "sep-by1 - collects the result in a vector; two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0775
  (let [s1 (parse (sep-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1*")]
    (testing "sep-by1 - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0780
  (let [p1 (sep-by1 (many k/white-space) (many letter))
        s1 (parse p1 "one two \t\t  three")]
    (testing "sep-by1 - three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "three")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0785
  (let [s1 (parse (sep-by1 (sym* \|) (>> k/upper digit)) "A1|B2|C3|DD,*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-by1 - compound item fails after reading several items"
      (is (= [\D \, \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\D\nexpecting digit"
                :cljs "unexpected \"D\"\nexpecting digit")
             em)))))


(deftest test-0790
  (let [s1 (parse (end-by (sym* \,) digit) "*")]
    (testing "end-by - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0800
  (let [s1 (parse (end-by (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0805
  (let [s1 (parse (end-by (sym* \,) digit) "0*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by - one item; with no separator it fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting \\,"
                :cljs "unexpected \"*\"\nexpecting \",\"")
             em)))))


(deftest test-0810
  (let [s1 (parse (end-by (sym* \,) (<*> k/upper digit)) "U2*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting \\,"
                :cslj "unexpected \"*\"\nexpecting \",\"")
             em)))))


(deftest test-0815
  (let [s1 (parse (end-by (sym* \,) (>> letter digit)) "U2,*")]
    (testing "end-by - there is one item that ends with a separator"
      (is (= [\*] (:input s1)))
      (is (= [\2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0820
  (let [s1 (parse (end-by (sym* \,) digit) "0,1,*")]
    (testing "end-by - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0820-05
  (let [s1 (parse (end-by (sym* \,) digit) "0,1,*")]
    (testing "end-by - collects the result in a vector; two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0825
  (let [s1 (parse (end-by (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (testing "end-by - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0830
  (let [p1 (end-by (many k/white-space) (many letter))
        s1 (parse p1 "one two \t\t  three\n")]
    (testing "end-by - three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "three")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0835
  (let [s1 (parse (end-by1 (sym* \,) digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by1 - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0840
  (let [s1 (parse (end-by1 (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by1 - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0845
  (let [s1 (parse (end-by1 (sym* \,) digit) "0*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by1 - one item; with no separator it fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting \\,"
                :cljs "unexpected \"*\"\nexpecting \",\"")
             em)))))


(deftest test-0850
  (let [s1 (parse (end-by1 (sym* \,) (<*> k/upper digit)) "U2*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by1 - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting \\,"
                :cljs "unexpected \"*\"\nexpecting \",\"")
             em)))))


(deftest test-0855
  (let [s1 (parse (end-by1 (sym* \,) (>> letter digit)) "U2,*")]
    (testing "end-by1 - there is one item and the separator"
      (is (= [\*] (:input s1)))
      (is (= [\2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0860
  (let [s1 (parse (end-by1 (sym* \,) digit) "0,1,*")]
    (testing "end-by1 - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0860-05
  (let [s1 (parse (end-by1 (sym* \,) digit) "0,1,*")]
    (testing "end-by1 - collects the result in a vector; two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0865
  (let [s1 (parse (end-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (testing "end-by1 - two compound items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0870
  (let [p1 (end-by1 (many k/white-space) (many letter))
        s1 (parse p1 "one two \t\t  three\n")]
    (testing "end-by1 - three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "three")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0880
  (let [s1 (parse (end-by1 (sym* \|) (>> k/upper digit)) "A1|B2|C3|DD,*")
        em (k/get-msg-str (:error s1))]
    (testing "end-by1 - compound item fails after reading several items"
      (is (= [\D \, \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\D\nexpecting digit"
                :cljs "unexpected \"D\"\nexpecting digit")
             em)))))


(deftest test-0885
  (let [s1 (parse (sep-end-by (sym* \,) digit) "*")]
    (testing "sep-end-by - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-0890
  (let [s1 (parse (sep-end-by (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-end-by - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0895
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0*")]
    (testing "sep-end-by - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0900
  (let [s1 (parse (sep-by (sym* \,) (<*> k/upper digit)) "U2*")]
    (testing "sep-by - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0905
  (let [s1 (parse (sep-end-by (sym* \,) (>> letter digit)) "U2,*")]
    (testing "sep-end-by - one item ended by the separator"
      (is (= [\*] (:input s1)))
      (is (= [\2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0910
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0,1*")]
    (testing "sep-end-by - two simple items separated by ,"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0910-05
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0,1*")]
    (testing "sep-end-by - collects the result in a vector; two simple items separated by ,"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))

(deftest test-0915
  (let [s1 (parse (sep-end-by (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (testing "sep-end-by - two compound items separated and ended by ,"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0920
  (let [p1 (sep-end-by (many1 k/white-space) (<*> letter letter letter))
        s1 (parse p1 "one two\t \tsix\n")]
    (testing "sep-end-by - three compound items; using many1 to avoid an SO"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "six")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0925
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-end-by1 - there are no separated items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0930
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> letter digit)) "A*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-end-by1 - there are no separated compound items"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting digit"
                :cljs "unexpected \"*\"\nexpecting digit")
             em)))))


(deftest test-0935
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "0*")]
    (testing "sep-end-by1 - one item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [\0] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0940
  (let [s1 (parse (sep-end-by1 (sym* \,) (<*> k/upper digit)) "U2*")]
    (testing "sep-end-by1 - one compound item, no separator"
      (is (= [\*] (:input s1)))
      (is (= [[\U \2]] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0950
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> letter digit)) "U2,*")]
    (testing "sep-end-by1 - one compound item ended by the separator"
      (is (= [\*] (:input s1)))
      (is (= [\2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0960
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "0,1*")]
    (testing "sep-end-by1 - two simple items"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0965
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (testing "sep-end-by1 - two compound items ended by the separator"
      (is (= [\*] (:input s1)))
      (is (= [\0 \1] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0970
  (let [p1 (sep-end-by1 k/white-space (<*> letter letter letter))
        s1 (parse p1 "one two\tsix\n")]
    (testing "sep-end-by1 - three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "six")) (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0970-05
  (let [p1 (sep-end-by1 k/white-space (<*> letter letter letter))
        s1 (parse p1 "one two\tsix\n")]
    (testing "sep-end-by1 - collects the result in a vector; three compound items"
      (is (empty? (:input s1)))
      (is (= (list (seq "one") (seq "two") (seq "six")) (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0970-10
  (let [p1 (sep-end-by1 k/white-space (many1 letter))
        s1 (parse p1 "one")]
    (testing "sep-end-by1 - collects the result in a vector; one compound item"
      (is (empty? (:input s1)))
      (is (= [(seq "one")] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0975
  (let [s1 (parse (sep-end-by1 (sym* \|) (>> k/upper digit)) "A1|B2|C3|DD,*")
        em (k/get-msg-str (:error s1))]
    (testing "sep-end-by1 - compound item fails after reading several items"
      (is (= [\D \, \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\D\nexpecting digit"
                :cljs "unexpected \"D\"\nexpecting digit")
             em)))))


(deftest test-0980
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "{0}*")]
    (testing "between - one item"
      (is (= [\*] (:input s1)))
      (is (= \0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0980-05
  (let [s1 (parse (between (sym* \:) digit) ":0:*")]
    (testing "between - with same delimiter - one item"
      (is (= [\*] (:input s1)))
      (is (= \0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0980-10
  (let [s1 (parse (between (sym* \|) (many digit)) "|5005|*")]
    (testing "between - with same delimiter - multiple items"
      (is (= [\*] (:input s1)))
      (is (= [\5 \0 \0 \5] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0985
  (let [p1 (>>= letter
                (fn [x] (>>= (many alpha-num)
                             (fn [y] (return (cons x y))))))
        s1 (parse (between (sym* \{) (sym* \}) p1) "{abc101z}*")]
    (testing "between - one compound item"
      (is (= [\*] (:input s1)))
      (is (= (seq "abc101z") (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-0990
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "(0}*")
        em (k/get-msg-str (:error s1))]
    (testing "between - the open parser fails"
      (is (= [\( \0 \} \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\(\nexpecting \\{"
                :cljs "unexpected \"(\"\nexpecting \"{\"")
             em)))))


(deftest test-0995
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "{0)*")
        em (k/get-msg-str (:error s1))]
    (testing "between - the close parser fails"
      (is (= [\) \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\)\nexpecting \\}"
                :cljs "unexpected \")\"\nexpecting \"}\"")
             em)))))


(deftest test-1000
  (let [s1 (parse (times 0 digit) "0*")]
    (testing "times - zero items"
      (is (= [\0 \*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1001
  (let [s1 (parse (times 1 letter) "x")]
    (testing "times - one item"
      (is (= [] (:input s1)))
      (is (= [\x] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1005
  (let [s1 (parse (times 3 (>> k/any-char letter)) "*a@b$c")]
    (testing "times - three items"
      (is (empty? (:input s1)))
      (is (= [\a \b \c] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1005-05
  (let [s1 (parse (times 3 (>> k/any-char letter)) "*a@b$c")]
    (testing "times - collects the result in a vector; three items"
      (is (empty? (:input s1)))
      (is (= [\a \b \c] (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1010
  (let [s1 (parse (times 3 (>> k/any-char letter)) "*a@b$$")
        em (k/get-msg-str (:error s1))]
    (testing "times - two items, then fails"
      (is (= [\$] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\$\nexpecting letter"
                :cljs "unexpected \"$\"\nexpecting letter")
             em)))))


(deftest test-1015
  (let [s1 (parse (<$> count (k/look-ahead (many digit))) "12345678")]
    (testing "look-ahead - succeeds consuming input"
      (is (= (seq "12345678") (:input s1)))
      (is (= 8 (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1020
  (let [s1 (parse (k/look-ahead (many digit)) "YYZ")]
    (testing "look-ahead - succeeds consuming no input"
      (is (= [\Y \Y \Z] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1025
  (let [s1 (parse (k/look-ahead digit) "*")]
    (testing "look-ahead - fails consuming no input"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1030
  (let [s1 (parse (k/look-ahead (>> letter digit)) "A*")]
    (testing "look-ahead - fails consuming no input"
      (is (= [\A \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1030-05
  (let [s1 (parse (k/predict (sym* \=)) "=10")]
    (testing "predict - if p succeeds it consumes no input"
      (is (= [\= \1 \0] (:input s1)))
      (is (= \= (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1030-10
  (let [s1 (parse (k/predict (sym* \=)) "<10")]
    (testing "predict - if p succeeds it consumes no input"
      (is (= [\< \1 \0] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-1035
  (let [s1 (parse (k/not-followed-by digit) "**")]
    (testing "not-followed-by - succeeds as digit fails"
      (is (= [\* \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1040
  (let [s1 (parse (k/not-followed-by (>> letter letter digit)) "xy**")]
    (testing "not-followed-by - succeeds as compound item fails"
      (is (= [\x \y \* \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1045
  (let [s1 (parse (k/not-followed-by (<*> k/upper digit)) "U2*")
        em (k/get-msg-str (:error s1))]
    (testing "not-followed-by - fails as the parse succeeds"
      (is (= [\U \2 \*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected [\\U \\2]"
                :cljs "unexpected [\"U\" \"2\"]")
             em)))))


(deftest test-1050
  (let [s1 (parse (>> k/upper k/upper k/upper eof) "YYZ")]
    (testing "eof - there's nothing left"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1055
  (let [s1 (parse eof "")]
    (testing "eof - there's nothing to begin with"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (:empty s1))
      (is (nil? (:error s1))))))


(deftest test-1060
  (let [s1 (parse (<*> k/upper digit eof) "U2*")
        em (k/get-msg-str (:error s1))]
    (testing "eof - fails because the input isn't empty"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "unexpected *\nexpecting end of input" em)))))


(deftest test-1065
  (let [s1 (parse (k/many-till digit letter) "123456A")]
    (testing "many-till - parses several numbers, then a letter"
      (is (empty? (:input s1)))
      (is (= (seq "123456") (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1065-05
  (let [s1 (parse (k/many-till digit letter) "123456A")]
    (testing "many-till - collects the result in a vector; several numbers and a letter"
      (is (empty? (:input s1)))
      (is (= (seq "123456") (:value s1)))
      (is (vector? (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1070
  (let [s1 (parse (k/many-till digit letter) "A*")]
    (testing "many-till - just the end parser"
      (is (= [\*] (:input s1)))
      (is (= [] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1075
  (let [p1 (>> (token* "<!--") (k/many-till k/any-char (<:> (token* "-->"))))
        s1 (parse p1 "<!-- -->")]
    (testing "many-till - reads a space between xml comments"
      (is (empty? (:input s1)))
      (is (= [\space] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1080
  (let [p1 (>> (token* "<!--") (k/many-till k/any-char (<:> (token* "-->"))))
        s1 (parse p1 "<!--foobar-->")]
    (testing "many-till - reads a word between xml comments"
      (is (empty? (:input s1)))
      (is (= (seq "foobar") (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1)))
      (is (nil? (:error s1))))))


(deftest test-1085
  (let [s1 (parse (k/many-till digit letter) "*A")
        em (k/get-msg-str (:error s1))]
    (testing "many-till - fails parsing the prefix"
      (is (= [\* \A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1))
      (is (= #?(:clj "unexpected \\*\nexpecting letter or digit"
                :cljs "unexpected \"*\"\nexpecting letter or digit")
             em)))))


(deftest test-1090
  (let [s1 (parse (k/many-till digit letter) "12345*A")
        em (k/get-msg-str (:error s1))]
    (testing "many-till - parses several prefixes and then fails"
      (is (= [\* \A] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting letter or digit"
                :cljs "unexpected \"*\"\nexpecting letter or digit")
             em)))))


(deftest test-1095
  (let [s1 (parse (k/many-till digit (>> k/upper (sym* \X))) "12345A*")
        em (k/get-msg-str (:error s1))]
    (testing "many-till - parses the prefix, then fails reading the end parser"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= #?(:clj "unexpected \\*\nexpecting \\X"
                :cljs "unexpected \"*\"\nexpecting \"X\"")
             em)))))


(deftest test-1113
  (let [s1 (parse (<+> letter) "a")]
    (testing "<+> - cats from one parser"
      (is (empty? (:input s1)))
      (is (= "a" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1114
  (let [s1 (parse (<+> (times 3 (sym* \S))) "SSS")]
    (testing "<+> - cats from one parser"
      (is (empty? (:input s1)))
      (is (= "SSS" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1115
  (let [s1 (parse (<+> (sym* \-) digit) "-1")]
    (testing "<+> - cats from two parsers"
      (is (empty? (:input s1)))
      (is (= "-1" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1120
  (let [s1 (parse (<+> (sym* \-) digit (sym* \;)) "-1;")]
    (testing "<+> - cats from three parsers"
      (is (empty? (:input s1)))
      (is (= "-1;" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1125
  (let [p1 (<+> letter (>> (sym* \|) letter) (>> (sym* \|) digit))
        s1 (parse p1 "X|Y|9")]
    (testing "<+> - cats from filtering parsers"
      (is (empty? (:input s1)))
      (is (= "XY9" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1130
  (let [in "ABC012"
        p1 (<+> (<*> letter letter letter) (<*> digit digit digit))
        s1 (parse p1 in)]
    (testing "<+> - cats from compound parsers"
      (is (empty? (:input s1)))
      (is (= "ABC012" (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1135
  (let [s1 (parse (<+> letter digit) "*")]
    (testing "<+> - the first parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (:empty s1)))))


(deftest test-1140
  (let [s1 (parse (<+> letter digit) "A*")]
    (testing "<+> - the second parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-1145
  (let [s1 (parse (<+> letter tab digit) "A\t*")]
    (testing "<+> - the third parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-1150
  (let [p1 (<+> letter tab tab tab tab tab (sym* \x))
        s1 (parse p1 "A\t\t\t\t\t*")]
    (testing "<+> - the seventh parser fails"
      (is (= [\*] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1))))))


(deftest test-1150-05
  (let [s1 (parse (k/search dec-num) "Now I have 20 dollars")]
    (testing "search - a simple number"
      (is (= (seq " dollars") (:input s1)))
      (is (= 20 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1150-10
  (let [s1 (parse (many (k/search dec-num)) "Now I have 20 dollars, or 2 tens")]
    (testing "search - multiple numbers"
      (is (empty? (:input s1)))
      (is (= [20 2] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1150-15
  (let [s1 (parse (many (k/search (<|> dec-num (token* "dollars")))) "Now I have 20 dollars")]
    (testing "search - multiple choices, multiple times"
      (is (empty? (:input s1)))
      (is (= [20 "dollars"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


;; +-------------------------------------------------------------+
;; |                        Parser state.                        |
;; +-------------------------------------------------------------+


(deftest test-1155
  (let [p1 (>> (k/put-state 0)
               (skip-many
                 (bind [x k/any-char]
                       (if (= x \newline) (k/modify-state inc) (return nil)))))
        s1 (parse p1 "aaa\nbbb\nccc\nddd\nfff\nggg\nhhh\n\niii\njjj\n\nkkk")]
    (testing "put-state, modify-state"
      (is (empty? (:input s1)))
      (is (nil? (:value s1)))
      (is (:ok s1))
      (is (= 11 (:user s1)))
      (is (false? (:empty s1))))))


(deftest test-1160
  (let [p1 (>> (skip (k/put-state 0)
                     (skip-many
                       (bind [x k/any-char]
                             (if (= x \newline) (k/modify-state inc) (return nil)))))
               k/get-state)
        s1 (parse p1 "aaa\nbbb\nccc\nddd\nfff\nggg\nhhh\n\niii\njjj\n\nkkk")]
    (testing "put-state, get-state; get user state as the parser's value"
      (is (empty? (:input s1)))
      (is (= 11 (:value s1)))
      (is (:ok s1))
      (is (= 11 (:user s1)))
      (is (false? (:empty s1))))))


(deftest test-1165
  (let [in "ABC"
        p1 (<+> letter letter letter)
        p2 (>> (k/set-input "XYZ") p1)
        s1 (parse (<*> p1 p2) in)]
    (testing "set-input"
      (is (empty? (:input s1)))
      (is (= ["ABC" "XYZ"] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1170
  (let [in "ABC"
        p1 (<+> letter letter letter)
        p2 (>> (skip (k/set-input "XY0")
                     (k/set-position (k/make-pos "include"))) p1)
        s1 (parse (<*> p1 p2) in)
        em (k/get-msg-str (:error s1))
        ip (:pos s1)]
    (testing "set-input, set-position"
      (is (= [\0] (:input s1)))
      (is (nil? (:value s1)))
      (is (false? (:ok s1)))
      (is (false? (:empty s1)))
      (is (= "include" (:src ip)))
      (is (= 1 (:line ip)))
      (is (= 3 (:col ip)))
      (is (= #?(:clj "unexpected \\0\nexpecting letter"
                :cljs "unexpected \"0\"\nexpecting letter")
             em)))))


(deftest test-1175
  (let [in "ABC"
        p1 (<+> letter letter letter)
        p2 (>> (skip (k/set-input "WXYZ")
                     (k/set-position (k/make-pos "include"))) k/get-position)
        s1 (parse (>> p1 p2) in)
        v1 (:value s1)]
    (testing "set-input, set-position, get-position"
      (is (= (seq "WXYZ") (:input s1)))
      (is (= "include" (:src v1)))
      (is (= 1 (:line v1)))
      (is (= 1 (:col v1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1180
  (let [in "ABC"
        p1 (<+> letter letter letter)
        p2 (>> (k/set-input "XYZ") k/get-input)
        s1 (parse (>> p1 p2) in)]
    (testing "set-input, get-input"
      (is (= [\X \Y \Z] (:input s1)))
      (is (= [\X \Y \Z] (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


;; +-------------------------------------------------------------+
;; |                      Numeric parsers.                       |
;; +-------------------------------------------------------------+


(deftest test-1185
  (let [s1 (parse dec-num "747")]
    (testing "dec-num - reads a simple integer"
      (is (empty? (:input s1)))
      (is (= 747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1190
  (let [s1 (parse dec-num "747-600")]
    (testing "dec-num - reads a simple integer, delimited"
      (is (= [\- \6 \0 \0] (:input s1)))
      (is (= 747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1195
  (let [s1 (parse (>> k/upper dec-num) "A380aircraft")]
    (testing "dec-num - reads an integer, delimited"
      (is (= (seq "aircraft") (:input s1)))
      (is (= 380 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1200
  (let [s1 (parse dec-num "987654321987654321000|")]
    (testing "dec-num - reads an integer, delimited"
      (is (= [\|] (:input s1)))
      (is (= 987654321987654321000N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1205
  (let [s1 (parse k/oct-num "0747")]
    (testing "oct-num - reads a simple octal number"
      (is (empty? (:input s1)))
      (is (= 0747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1210
  (let [s1 (parse k/oct-num "0747-600")]
    (testing "oct-num - reads a simple octal number, delimited"
      (is (= [\- \6 \0 \0] (:input s1)))
      (is (= 0747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1215
  (let [s1 (parse (>> k/upper k/oct-num) "B767aircraft")]
    (testing "oct-num - reads an octal number, delimited"
      (is (= (seq "aircraft") (:input s1)))
      (is (= 0767 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1220
  (let [s1 (parse k/oct-num "76543217654321000000|")]
    (testing "oct-num - reads an octal number, delimited"
      (is (= [\|] (:input s1)))
      (is (= 076543217654321000000N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1225
  (let [s1 (parse k/hex-num "747")]
    (testing "hex-num - reads a simple hex number"
      (is (empty? (:input s1)))
      (is (= 0x747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1230
  (let [s1 (parse k/hex-num "747-600")]
    (testing "hex-num - reads a simple hex number, delimited"
      (is (= [\- \6 \0 \0] (:input s1)))
      (is (= 0x747 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1235
  (let [s1 (parse (>> k/upper k/hex-num) "A380plane")]
    (testing "hex-num - reads a hex number, delimited"
      (is (= (seq "plane") (:input s1)))
      (is (= 0x380 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1240
  (let [s1 (parse k/hex-num "ABCDEF987654321987654321000|")]
    (testing "hex-num - reads a hex number, delimited"
      (is (= [\|] (:input s1)))
      (is (= 0xABCDEF987654321987654321000N (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1245
  (let [s1 (parse k/float-num "100")]
    (testing "float-num - reads a simple floating-point number"
      (is (empty? (:input s1)))
      (is (= 100.0 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1250
  (let [s1 (parse k/float-num "3.1415927")]
    (testing "float-num - reads a simple floating-point number"
      (is (empty? (:input s1)))
      (is (= 3.1415927 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1255
  (let [s1 (parse k/float-num "9.8m/s")]
    (testing "float-num - reads a simple floating-point number, delimited"
      (is (= [\m \/ \s] (:input s1)))
      (is (= 9.8 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1260
  (let [s1 (parse k/float-num "0.00343ms")]
    (testing "float-num - reads a floating-point number, delimited"
      (is (= [\m \s] (:input s1)))
      (is (= 0.00343 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))


(deftest test-1265
  (let [s1 (parse k/float-num "98765432.19876543555666|")]
    (testing "float-num - reads a floating-point number, delimited"
      (is (= [\|] (:input s1)))
      (is (= 9.876543219876544E7 (:value s1)))
      (is (:ok s1))
      (is (false? (:empty s1))))))
