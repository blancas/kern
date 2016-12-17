;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-core
  (:use [blancas.kern.core]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))

;; Private functions from kern.core

(def make-err-message  (ns-resolve 'blancas.kern.core 'make-err-message)) 
(def make-err-unexpect (ns-resolve 'blancas.kern.core 'make-err-unexpect)) 
(def make-err-system   (ns-resolve 'blancas.kern.core 'make-err-system)) 
(def get-msg-str       (ns-resolve 'blancas.kern.core 'get-msg-str)) 
(def make-pos          (ns-resolve 'blancas.kern.core 'make-pos))


;; +-------------------------------------------------------------+
;; |                       Basic parsers.                        |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse letter "xyz")
	s2 ((return 0) s1)]
    (fact "return"
	  (:value s2)  =>  0
	  (:ok    s2)  =>  true
	  (:error s2)  =>  nil
	  (:input s2)  =>  (:input s1)
	  (:empty s2)  =>  (:empty s1)
	  (:pos   s2)  =>  (:pos   s1)
	  (:user  s2)  =>  (:user  s1))))


(deftest test-0005
  (let [em "the buck stops here"
	s1 (parse letter "xyz")
	s2 ((fail em) s1)]
    (fact "fail"
	  (:value s2)  =>  nil
	  (:ok    s2)  =>  false
	  (:empty s2)  =>  true
	  (:error s2)  =>  (make-err-message (:pos s1) em)
	  (:input s2)  =>  (:input s1)
	  (:pos   s2)  =>  (:pos   s1)
	  (:user  s2)  =>  (:user  s1))))


(deftest test-0010
  (let [s1 (parse letter "xyz")
	s2 ((satisfy #(Character/isLetter %)) s1)]
    (fact "satisfy - advances one char"
	  (:input s2)  =>  [\z]
	  (:value s2)  =>  \y
	  (:ok    s2)  =>  true
	  (:empty s2)  =>  false
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  nil
	  (:pos   s2)  =>  (contains {:line 1 :col 3}))))


(deftest test-0015
  (let [s1 (parse letter "u2")
	s2 ((satisfy #(Character/isDigit %)) s1)]
    (fact "satisfy - reaches the end of input"
	  (:input s2)  =>  empty?
	  (:value s2)  =>  \2
	  (:ok    s2)  =>  true
	  (:empty s2)  =>  false
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  nil
	  (:pos   s2)  =>  (contains {:line 1 :col 3}))))


(deftest test-0020
  (let [s1 (parse letter "u\t")
	s2 ((satisfy #(= \tab %)) s1)]
    (fact "satisfy - advnaces one tab; default 4 positions"
	  (:value s2)  =>  \tab
	  (:ok    s2)  =>  true
	  (:empty s2)  =>  false
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  nil
	  (:pos   s2)  =>  (contains {:line 1 :col 6}))))


(deftest test-0025
  (binding [*tab-width* 8]
    (let [s1 (parse letter "u\t")
	  s2 ((satisfy #(= \tab %)) s1)]
      (fact "satisfy - advances one tab of 8 positions"
	    (:value s2)  =>  \tab
	    (:ok    s2)  =>  true
	    (:empty s2)  =>  false
	    (:user  s2)  =>  (:user s1)
	    (:error s2)  =>  nil
	    (:pos   s2)  =>  (contains {:line 1 :col 10})))))


(deftest test-0030
  (let [s1 (parse letter "u\n")
	s2 ((satisfy #(= \newline %)) s1)]
    (fact "satisfy - advances to the next line, first column"
	  (:value s2)  =>  \newline
	  (:ok    s2)  =>  true
	  (:empty s2)  =>  false
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  nil
	  (:pos   s2)  =>  (contains {:line 2 :col 1}))))


(deftest test-0035
  (let [em "end of input"
	s1 (parse (many letter) "xyz")
	s2 ((satisfy (fn [_] true)) s1)]
    (fact "satisfy - attempts to read past the end of input"
	  (:input s2)  =>  empty?
	  (:value s2)  =>  nil
	  (:ok    s2)  =>  false
	  (:empty s2)  =>  true
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  (make-err-unexpect (:pos s1) em)
	  (:pos   s2)  =>  (:pos s1))))


(deftest test-0040
  (let [em "\\2"
	s1 (parse letter "u2")
	s2 ((satisfy #(Character/isLetter %)) s1)]
    (fact "satisfy - the predicate fails"
	  (:input s2)  =>  [\2]
	  (:value s2)  =>  nil
	  (:ok    s2)  =>  false
	  (:empty s2)  =>  true
	  (:user  s2)  =>  (:user s1)
	  (:error s2)  =>  (make-err-system (:pos s1) em)
	  (:pos   s2)  =>  (:pos s1))))


;; +-------------------------------------------------------------+
;; |                    Primitive parsers.                       |
;; +-------------------------------------------------------------+


(deftest test-0045
  (let [in "#(f %)"
	s1 (parse any-char in)]
    (fact "any-char"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \#
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  (:user s1)
	  (:error s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 2}))))


(deftest test-0050
  (let [in "xyz"
	s1 (parse (>>= any-char
		       (fn [a]
		         (>>= any-char
			      (fn [b]
				(>>= any-char
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "any-char - three in a row"
	  (:value s1)  =>  (seq in))))


(deftest test-0055
  (let [em "end of input"
	s1 (parse any-char "")]
    (fact "any-char - fails on end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	  (:user  s1)  =>  (:user s1)
	  (:error s1)  =>  (make-err-unexpect (:pos s1) em)
	  (:pos   s1)  =>  (contains {:line 1 :col 1}))))


(deftest test-0060
  (let [in "abc"
	s1 (parse letter in)]
    (fact "letter - parses a single letter"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "letter - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0070
  (let [in "123"
	s1 (parse letter in)]
    (fact "letter - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0075
  (let [in "abc"
	s1 (parse lower in)]
    (fact "lower - parses a single lower-case letter"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0080
  (let [in "xyz"
	s1 (parse (>>= lower
		       (fn [a]
		         (>>= lower
			      (fn [b]
				(>>= lower
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "lower - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0085
  (let [in "XYZ"
	s1 (parse lower in)]
    (fact "lower - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0090
  (let [in "ABC"
	s1 (parse upper in)]
    (fact "upper - parses a single upper-case letter"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0095
  (let [in "XYZ"
	s1 (parse (>>= upper
		       (fn [a]
		         (>>= upper
			      (fn [b]
				(>>= upper
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "upper - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0100
  (let [in "123"
	s1 (parse upper in)]
    (fact "upper - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0105
  (let [in " \t\t"
	s1 (parse white-space in)]
    (fact "white-space - parses a single whitespace character"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \space
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0110
  (let [in " \t "
	s1 (parse (>>= white-space
		       (fn [a]
		         (>>= white-space
			      (fn [b]
				(>>= white-space
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "white-space - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0115
  (let [in "***"
	s1 (parse white-space in)]
    (fact "white-space - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0120
  (let [in "   "
	s1 (parse space in)]
    (fact "space - parses a single space character"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \space
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "space - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0130
  (let [in "***"
	s1 (parse space in)]
    (fact "space - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0135
  (let [in "\n\t."
	s1 (parse new-line* in)]
    (fact "new-line - parses a single newline character"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \newline
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "new-line* - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0145
  (let [in "***"
	s1 (parse new-line* in)]
    (fact "new-line* - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0150
  (let [in "\t|\t|"
	s1 (parse tab in)]
    (fact "tab - parses a single tab character"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "tab - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0160
  (let [in "***"
	s1 (parse tab in)]
    (fact "tab - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0165
  (let [in "12345"
	s1 (parse digit in)]
    (fact "digit - parses a single digit"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \1
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "digit - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0175
  (let [in "***"
	s1 (parse digit in)]
    (fact "digit - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0180
  (let [in "ABCDEF"
	s1 (parse hex-digit in)]
    (fact "hex-digit - parses a single hex digit"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0185
  (let [in "CAB"
	s1 (parse (>>= hex-digit
		       (fn [a]
		         (>>= hex-digit
			      (fn [b]
				(>>= hex-digit
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "hex-digit - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0190
  (let [in "***"
	s1 (parse digit in)]
    (fact "hex-digit - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0195
  (let [in "12345"
	s1 (parse oct-digit in)]
    (fact "oct-digit - parses a single octal digit"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \1
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0200
  (let [in "567"
	s1 (parse (>>= oct-digit
		       (fn [a]
		         (>>= oct-digit
			      (fn [b]
				(>>= oct-digit
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "oct-digit - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0205
  (let [in "***"
	s1 (parse digit in)]
    (fact "oct-digit - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0210
  (let [in "a0b1"
	s1 (parse alpha-num in)]
    (fact "alpha-num - parses a single alpha-numeric character"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0215
  (let [in "a1b"
	s1 (parse (>>= alpha-num
		       (fn [a]
		         (>>= alpha-num
			      (fn [b]
				(>>= alpha-num
				     (fn [c]
				       (return [a b c])))))))
		  in)]
    (fact "alpha-num - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0220
  (let [in "+*&"
	s1 (parse alpha-num in)]
    (fact "alpha-num - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0225
  (let [in "X()"
	s1 (parse (sym* \X) in)]
    (fact "sym* - parses a single X"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "sym* - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0235
  (let [in "***"
	s1 (parse (sym* \X) in)]
    (fact "sym* - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0235-05
  (let [in "x()"
	s1 (parse (sym- \X) in)]
    (fact "sym- - parses a single x"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0235-10
  (let [in "X()"
	s1 (parse (sym- \X) in)]
    (fact "sym- - parses a single X"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0235-15
  (let [in "A()"
	s1 (parse (sym- \X) in)
	em (get-msg-str (:error s1))]
    (fact "sym- - parses a single X"
	  (:input s1)  =>  [\A \( \)]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
                   em  =>  "unexpected \\A\nexpecting \\X")))


(deftest test-0240
  (let [in "program foo()"
	s1 (parse (token* "program") in)]
    (fact "token* - parses a specific word"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "token* - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1) => ["foo" "(bar)" "baz"])))


(deftest test-0250
  (let [in "goat"
	s1 (parse (token* "goal") in)
	em (get-msg-str (:error s1))]
    (fact "token* - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected goat\nexpecting goal")))


(deftest test-0250-05
  (let [in "function foo()"
	s1 (parse (token* "function" "procedure") in)]
    (fact "token* - parses one of multiple word choices"
	  (:input s1)  =>  (drop (count "function") in)
	  (:value s1)  =>  "function"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-10
  (let [in "procedure foo()"
	s1 (parse (token* "function" "procedure") in)]
    (fact "token* - parses one of multiple word choices"
	  (:input s1)  =>  (drop (count "procedure") in)
	  (:value s1)  =>  "procedure"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-15
  (let [in "program foo()"
	s1 (parse (token- "PROGRAM") in)]
    (fact "token- - parses a specific word; non case-sensetive"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "PROGRAM"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-20
  (let [in "Program foo()"
	s1 (parse (token- "PROGRAM") in)]
    (fact "token- - parses a specific word; non case-sensetive"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "PROGRAM"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-25
  (let [in "PROGRAM foo()"
	s1 (parse (token- "PROGRAM") in)]
    (fact "token- - parses a specific word; non case-sensetive"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "PROGRAM"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-30
  (let [in "goat"
	s1 (parse (token- "goal") in)
	em (get-msg-str (:error s1))]
    (fact "token- - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected goat\nexpecting goal")))


(deftest test-0250-35
  (let [in "FUNction foo()"
	s1 (parse (token- "function" "procedure") in)]
    (fact "token- - parses one of multiple word choices"
	  (:input s1)  =>  (drop (count "function") in)
	  (:value s1)  =>  "function"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-40
  (let [in "PROCedure foo()"
	s1 (parse (token- "function" "procedure") in)]
    (fact "token- - parses one of multiple word choices"
	  (:input s1)  =>  (drop (count "procedure") in)
	  (:value s1)  =>  "procedure"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-45
  (let [in "program foo()"
	s1 (parse (word* letter "program") in)]
    (fact "word* - parses a specific, delimited word"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-50
  (let [in "else{}"
	s1 (parse (word* letter "else") in)]
    (fact "word* - parses a specific, delimited word"
	  (:input s1)  =>  [\{ \}]
	  (:value s1)  =>  "else"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-55
  (let [in "procedure"
	s1 (parse (word* letter "proc") in)
	em (get-msg-str (:error s1))]
    (fact "word* - fails because is not delimited"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected e\nexpecting end of proc")))


(deftest test-0250-60
  (let [in "otherwise{}"
	s1 (parse (word* letter "else" "otherwise") in)]
    (fact "word* - parses a specific, delimited word"
	  (:input s1)  =>  [\{ \}]
	  (:value s1)  =>  "otherwise"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-65
  (let [in "subroutine"
	s1 (parse (word* letter "proc" "func" "method") in)
	em (get-msg-str (:error s1))]
    (fact "word* - fails with incorrect input"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected subr\nunexpected subrou\nexpecting proc, func or method")))


(deftest test-0250-70
  (let [in "PROGRAM foo()"
	s1 (parse (word- letter "program") in)]
    (fact "word- - parses a specific, delimited word; not case-senstive"
	  (:input s1)  =>  (drop (count "program") in)
	  (:value s1)  =>  "program"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-75
  (let [in "Else{}"
	s1 (parse (word- letter "else") in)]
    (fact "word- - parses a specific, delimited word; not case-senstive"
	  (:input s1)  =>  [\{ \}]
	  (:value s1)  =>  "else"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-80
  (let [in "ProcEdure"
	s1 (parse (word- letter "proc") in)
	em (get-msg-str (:error s1))]
    (fact "word- - fails because is not delimited"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected E\nexpecting end of proc")))


(deftest test-0250-85
  (let [in "OtherWise{}"
	s1 (parse (word- letter "else" "otherwise") in)]
    (fact "word- - parses a specific, delimited word; not case-senstive"
	  (:input s1)  =>  [\{ \}]
	  (:value s1)  =>  "otherwise"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0250-90
  (let [in "SUBroutine"
	s1 (parse (word- letter "proc" "func" "method") in)
	em (get-msg-str (:error s1))]
    (fact "word- - fails with incorrect input"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected SUBr\nunexpected SUBrou\nexpecting proc, func or method")))


(deftest test-0255
  (let [in "* 2"
	s1 (parse (one-of* "+-*/^") in)]
    (fact "one-of* - parses one of the supplied characters"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \*
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "one-of* - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0265
  (let [in "abc"
	s1 (parse (one-of* "+-*/") in)]
    (fact "one-of* - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0270
  (let [in ": 2"
	s1 (parse (none-of* "+-*/^") in)]
    (fact "none-of* - parses a character not supplied"
	  (:input s1)  =>  (rest in)
	  (:value s1)  =>  \:
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


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
    (fact "none-of* - three in a row until end of input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq in))))


(deftest test-0280
  (let [in "$foo"
	s1 (parse (none-of* "!@#$%^*()") in)]
    (fact "none-of* - fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0280-05
  (let [in ""
	s1 (parse eof in)]
    (fact "eof - parses an empty string"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true)))


(deftest test-0280-10
  (let [in "END."
	s1 (parse (>> (token* "END.") eof) in)]
    (fact "eof - verifies that input ends"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-15
  (let [in "END.// the end"
	s1 (parse (>> (token* "END.") eof) in)
	em (get-msg-str (:error s1))]
    (fact "eof - verifies that input ends"
	  (:input s1)  =>  (seq "// the end")
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  =>  "unexpected /\nexpecting end of input")))


(deftest test-0280-20
  (let [in "12\n"
	s1 (parse (<*> digit digit new-line*) in)]
    (fact "new-line* - a new line after two digits"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\1 \2 \newline]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-25
  (let [in "AB\r\nCD\r\n"
	s1 (parse (many1 (<< (many1 upper) new-line*)) in)]
    (fact "new-line* - pairs of letters separated by a new line"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [[\A \B] [\C \D]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-30
  (let [in "12345 "
	s1 (parse (<< (many1 digit) new-line*) in)
	em (get-msg-str (:error s1))]
    (fact "new-line* - the line doesn't end with a new line"
	  (:input s1)  =>  [\space]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  =>  "unexpected \\space\nexpecting new line")))


(deftest test-0280-35
  (let [in "   \t \t \n \t *"
	s1 (parse (skip-ws (sym* \*)) in)]
    (fact "skip-ws - skips whitespaces before parsing a star"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \*
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-40
  (let [in "*"
	s1 (parse (skip-ws (sym* \*)) in)]
    (fact "skip-ws - nothing to skip before parsing a star"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \*
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-45
  (let [in "Now is the time...  right... now."
	s1 (parse (field* "!") in)]
    (fact "field* - reads the whole string"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  in
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-50
  (let [in "Now is the time; right... now."
	s1 (parse (field* ";") in)]
    (fact "field* - reads the field delimited by a semicolon"
	  (:input s1)  =>  (seq "; right... now.")
	  (:value s1)  =>  "Now is the time"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-55
  (let [in "Now-is-the-time"
	s1 (parse (split-on "-") in)]
    (fact "field - breaks the string into the words"
	  (:value s1)  =>  ["Now" "is" "the" "time"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


  (deftest test-0280-60
    (let [in "Software,Tooling,495.95,0.00,,15,,xyz"
	  s1 (parse (split-on ",") in)]
      (fact "field - breaks the string into fields; some are empty"
	    (:value s1)  =>  ["Software" "Tooling" "495.95" "0.00" "15" "xyz"]
	    (:ok    s1)  =>  true
	    (:empty s1)  =>  false)))


(deftest test-0280-65
  (let [in "Now is the time.   Or, is it?     Yes! yes! that's it."
	s1 (parse (split-on " ,?!.") in)]
    (fact "field - collects all words; skips the given punctuation"
	  (:value s1)  =>  ["Now" "is" "the" "time" "Or" "is" "it" "Yes" "yes" "that's" "it"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0280-70
  (fact "mark parses a punctuation mark."
	(value mark "!") => \!
	(value mark "@") => \@
	(value mark "*") => \*
	(value mark ":") => \:
	(value mark "/") => \/
	(value mark ".") => \.))


;; +-------------------------------------------------------------+
;; |                     Parser combinators.                     |
;; +-------------------------------------------------------------+


(deftest test-0285
  (let [s1 (parse letter "1")
	em (-> s1 :error :msgs first :text force)]
    (fact "first message in the msgs list"
          em  =>  "letter")))


(deftest test-0290
  (let [s1 (parse letter "1")
	em (get-msg-str (:error s1))]
    (fact "verify error messages"
	  em  =>  "unexpected \\1\nexpecting letter")))


(deftest test-0295
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "01")
    	em (-> s1 :error :msgs first :text force)]
    (fact "<?> - does not add message when input is consumed"
	  em  =not=>  "digit,letter")))


(deftest test-0300
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "01")
    	em (get-msg-str (:error s1))]
    (fact "<?> - verifies error messages in <*>"
	  em  =>  "unexpected \\1\nexpecting letter")))


(deftest test-0305
  (let [s1 (parse (<?> (<*> digit letter) "digit,letter") "0")
    	em (get-msg-str (:error s1))]
    (fact "<?> - verifies error messages in <*>"
	  em  =>  "unexpected end of input\nexpecting letter")))


(deftest test-0310
  (let [s1 (parse (<|> digit letter) "*")
	em (get-msg-str (:error s1))]
    (fact "<|> - verifies error messages"
	  em  =>  "unexpected \\*\nexpecting digit or letter")))


(deftest test-0315
  (let [s1 (parse (<|> (sym* \x) (<|> letter digit)) "*")
	em (get-msg-str (:error s1))]
    (fact "<|> - verifies error messages with 3 choices"
	  em  =>  "unexpected \\*\nexpecting \\x, letter or digit")))


(deftest test-0320
  (let [s1 (parse (<|> (<|> white-space (sym* \x)) (<|> letter digit)) "*")
	em (get-msg-str (:error s1))]
    (fact "<|> - verifies error messages with 4 choices"
	  em  =>  "unexpected \\*\nexpecting whitespace, \\x, letter or digit")))


(deftest test-0320-05
  (let [s1 (parse (expect (<+> letter digit) "number two") "U2")]
    (fact "expect - parser succeeds"
      	  (:value s1)  =>  "U2"
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0320-10
  (let [s1 (parse (expect (<+> letter digit) "number two") "UX")
	em (get-msg-str (:error s1))]
    (fact "expect - parser fails consuming input"
      	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
                   em  =>  "unexpected \\X\nexpecting number two")))
	  

(deftest test-0320-15
  (let [s1 (parse (expect (<+> letter digit) "number two") "007")
	em (get-msg-str (:error s1))]
    (fact "expect - parser fails without consuming any input"
      	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	  (:input s1)  =>  [\0 \0 \7]
                   em  =>  "unexpected \\0\nexpecting number two")))


(deftest test-0325
  (let [s1 (parse (<|> letter digit) "U2")]
    (fact "<|> - the first parser succeeds"
      	  (:value s1)  =>  \U
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\2]
	  (:empty s1)  =>  false)))


(deftest test-0330
  (let [s1 (parse (<|> digit letter) "XYZ")]
    (fact "<|> - the second parser succeeds"
      	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  [\Y \Z]
	  (:empty s1)  =>  false)))


(deftest test-0335
  (let [s1 (parse (<|> (>> letter digit) letter) "XYZ")
	em (get-msg-str (:error s1))]
    (fact "<|> - the first parse fails consuming input"
      	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:input s1)  =>  [\Y \Z]
	  (:empty s1)  =>  false
                   em  =>  "unexpected \\Y\nexpecting digit")))


(deftest test-0340
  (let [s1 (parse (<|> white-space letter digit) "*")
	em (get-msg-str (:error s1))]
    (fact "<|> - verifies error messages with 3 choices"
	  em  =>  "unexpected \\*\nexpecting whitespace, letter or digit")))


(deftest test-0345
  (let [s1 (parse (<|> white-space (sym* \x) letter digit) "*")
	em (get-msg-str (:error s1))]
    (fact "<|> - verifies error messages with 4 choices"
	  em  =>  "unexpected \\*\nexpecting whitespace, \\x, letter or digit")))


(deftest test-0350
  (let [s1 (parse (<|> white-space (sym* \x) letter digit) "\t")]
    (fact "<|> - the first of 4 parser succeeds"
      	  (:value s1)  =>  \tab
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0355
  (let [s1 (parse (<|> white-space (sym* \x) letter digit) "x")]
    (fact "<|> - the second of 4 parser succeeds"
      	  (:value s1)  =>  \x
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0360
  (let [s1 (parse (<|> white-space (sym* \x) letter digit) "z")]
    (fact "<|> - the third of 4 parser succeeds"
      	  (:value s1)  =>  \z
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0365
  (let [s1 (parse (<|> white-space (sym* \x) letter digit) "0")]
    (fact "<|> - the fourth parser succeeds"
      	  (:value s1)  =>  \0
	  (:ok    s1)  =>  true
	  (:error s1)  =>  nil
	  (:input s1)  =>  empty?
	  (:empty s1)  =>  false)))


(deftest test-0370
  (let [p1 (>>= letter (fn [x] (return (Character/toUpperCase x))))
	s1 (parse p1 "xyz")]
    (fact ">>= - advances one char"
	  (:input s1)  =>  [\y \z]
	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 2}))))


(deftest test-0375
  (let [p1 (>>= digit
		(fn [x] (>>= digit
			     (fn [y] (return (Integer/parseInt (str x y)))))))
	s1 (parse p1 "50113")]
    (fact ">>= - advances two chars"
	  (:input s1)  =>  [\1 \1 \3]
	  (:value s1)  =>  50
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 3}))))


(deftest test-0380
  (let [in "012345"
	p1 (>>= letter (fn [x] (return (int x))))
	s1 (parse p1 in)
	em (get-msg-str (:error s1))]
    (fact ">>= - the first parser fails"
	  (:input s1)  =>  (seq in)
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	  (:user  s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 1})
	           em  => "unexpected \\0\nexpecting letter")))


(deftest test-0385
  (let [p1 (>>= letter (fn [_] digit))
	s1 (parse p1 "xyz")
	em (get-msg-str (:error s1))]
    (fact ">>= - the second parser fails"
	  (:input s1)  =>  [\y \z]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 2})
	           em  => "unexpected \\y\nexpecting digit")))


(deftest test-0385-05
  (let [p1 (bind [x letter] (return (Character/toUpperCase x)))
	s1 (parse p1 "xyz")]
    (fact "bind - advances one char"
	  (:input s1)  =>  [\y \z]
	  (:value s1)  =>  \X
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 2}))))


(deftest test-0385-10
  (let [p1 (bind [x digit y digit]
	     (return (Integer/parseInt (str x y))))
	s1 (parse p1 "50113")]
    (fact "bind - advances two chars"
	  (:input s1)  =>  [\1 \1 \3]
	  (:value s1)  =>  50
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil
	  (:pos   s1)  =>  (contains {:line 1 :col 3}))))


(deftest test-0385-15
  (let [p1 (bind [_ (sym* \()
		  s (<+> (many1 digit))
		  _ (sym* \))]
	     (return (* (Integer/parseInt s) -1)))
	s1 (parse p1 "(50113)")]
    (fact "bind - reads a negative number in parens, as in accounting"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  -50113
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil)))


(deftest test-0385-20
  (let [p1 (bind [x letter]
	     (if (= x \x)
	       (bind [y (sym* \Y)
		      z (sym* \Z)] (return "first"))
	       (bind [n (many1 digit)] (return 5005))))
	s1 (parse p1 "xYZ")]
    (fact "bind - uses nested bind inside the first function body"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "first"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil)))


(deftest test-0385-25
  (let [p1 (bind [x letter]
	     (if (= x \x)
	       (bind [y (sym* \Y)
		      z (sym* \Z)] (return "first"))
	       (bind [n (many1 digit)] (return 666))))
	s1 (parse p1 "A10002450")]
    (fact "bind - uses nested bind inside the first function body"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  666
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil)))


(deftest test-0385-30
  (let [p1 (<|> (bind [x (many1 digit)] (return true))
		(bind [x (many1 letter)] (return false)))
	s1 (parse p1 "FALSE")]
    (fact "bind - the first bind fails, the second succeeds"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  false
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:user  s1)  =>  nil
	  (:error s1)  =>  nil)))


(deftest test-0390
  (let [p1 (>> (sym* \+) digit)
	s1 (parse p1 "+1")]
    (fact ">> - consumes two chars, keeps the second"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \1
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0395
  (let [p1 (>> (sym* \+) digit)
	s1 (parse p1 "01")]
    (fact ">> - the first parser fails"
	  (:input s1)  =>  [\0 \1]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0400
  (let [p1 (>> (sym* \+) digit)
	s1 (parse p1 "+A")]
    (fact ">> - the second parser fails"
	  (:input s1)  =>  [\A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0405
  (let [p1 (>> digit digit letter)
	s1 (parse p1 "01A")]
    (fact ">> - consumes three chars, keeps the last"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0410
  (let [p1 (>> digit digit digit letter)
	s1 (parse p1 "012A")]
    (fact ">> - consumes four chars, keeps the last"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0415
  (let [p1 (>> digit digit digit letter)
	s1 (parse p1 "A")]
    (fact ">> - the first fails"
	  (:input s1)  =>  [\A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0420
  (let [p1 (>> digit digit digit letter)
	s1 (parse p1 "01A")]
    (fact ">> - the third fails"
	  (:input s1)  =>  [\A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0425
  (let [p1 (<< letter (sym* \;))
	s1 (parse p1 "a;")]
    (fact "<< - consumes two chars, keeps the first"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0430
  (let [p1 (<< letter (sym* \;))
	s1 (parse p1 "0;")]
    (fact "<< - the first parser fails"
	  (:input s1)  =>  [\0 \;]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0435
  (let [p1 (<< letter (sym* \;))
	s1 (parse p1 "A*")]
    (fact "<< - the second parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0440
  (let [p1 (<< any-char digit digit)
	s1 (parse p1 "+01")]
    (fact "<< - consumes three chars, keeps the first"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \+
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0445
  (let [p1 (<< any-char digit digit digit digit)
	s1 (parse p1 "+0123")]
    (fact "<< - consumes five chars, keeps the first"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \+
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0450
  (let [p1 (<< any-char digit digit digit digit)
	s1 (parse p1 "+01*")]
    (fact "<< - the fourth parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0455
  (let [s1 (parse (<$> count (many any-char)) "abcdef+01234*")]
    (fact "<$> - counts the length of the input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  13
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0460
  (let [s1 (parse (<$> #(- (int %) (int \0)) digit) "9")]
    (fact "<$> - converts a char digit into an int"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  9
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0465
  (let [s1 (parse (<$> #(- (int %) (int \0)) digit) "A")]
    (fact "<$> - fails and the function is not applied"
	  (:input s1)  =>  [\A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0469-a
  (let [s1 (parse (<*> digit) "9")]
    (fact "<*> - collects from one parser"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\9]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0469-b
  (let [s1 (parse (<*> letter) "U2")]
    (fact "<*> - collects from one parser"
	  (:input s1)  =>  [\2]
	  (:value s1)  =>  [\U]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0470
  (let [s1 (parse (<*> (sym* \-) digit) "-1")]
    (fact "<*> - collects from two parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\- \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0475
  (let [s1 (parse (<*> (sym* \-) digit (sym* \;)) "-1;")]
    (fact "<*> - collects from three parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\- \1 \;]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0480
  (let [p1 (<*> letter (>> (sym* \|) letter) (>> (sym* \|) digit))
	s1 (parse  p1 "X|Y|9")]
    (fact "<*> - collects from filtering parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\X \Y \9]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0485
  (let [in "ABC012"
	p1 (<*> (<*> letter letter letter) (<*> digit digit digit))
	s1 (parse  p1 in)]
    (fact "<*> - collects from compound parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  '((\A \B \C) (\0 \1 \2))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0485-05
  (let [s1 (parse (<*> letter digit) "u2")]
    (fact "<*> - collects results in a vector"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  vector?
	  (:value s1)  =>  [\u \2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0490
  (let [s1 (parse (<*> letter digit) "*")]
    (fact "<*> - the first parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0495
  (let [s1 (parse (<*> letter digit) "A*")]
    (fact "<*> - the second parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0500
  (let [s1 (parse (<*> letter tab digit) "A\t*")]
    (fact "<*> - the third parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0505
  (let [p1 (<*> letter tab tab tab tab tab (sym* \x))
	s1 (parse  p1 "A\t\t\t\t\t*")]
    (fact "<*> - the seventh parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-0510
  (let [s1 (parse (<:> lower) "a")]
    (fact "<:> - parses an item; consumes all input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \a
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0515
  (let [s1 (parse (<:> (<$> (partial apply str) (<*> (token* "end") space))) "end ")]
    (fact "<:> - parses nested items; consumes all input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "end "
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0520
  (let [in "a1b3c4d5e7f8"
	p1 (<*> letter digit)
	s1 (parse (<:> (many p1)) in)]
    (fact "<:> - parses six pairs; consumes all input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0525
  (let [s1 (parse (<:> lower) "*&!")]
    (fact "<:> - fails with parsers consuming no input; consumes no input"
	  (:input s1)  =>  [\* \& \!]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-0530
  (let [s1 (parse (<:> (<*> upper lower upper)) "Mi*")
        em (get-msg-str (:error s1))]
    (fact "<:> - fails with parsers consuming input; consumes no input"
	  (:input s1)  =>  [\M \i \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting uppercase letter")))


(deftest test-0535
  (let [s1 (parse (<|> (<:> (>> digit letter)) digit) "1*")
        em (get-msg-str (:error s1))]
    (fact "<:> - verifies that it allows <|> to test the next choice"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  \1
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0540
  (let [s1 (parse (<|> (<:> letter) digit) "***")
        em (get-msg-str (:error s1))]
    (fact "<:> - verifies that it carries over the error msg"
	  (:input s1)  =>  [\* \* \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting letter or digit")))


(deftest test-0545
  (let [s1 (parse (many lower) "*")]
    (fact "many - parses zero items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  empty?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0550
  (let [s1 (parse (many lower) "a*")]
    (fact "many - parses one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\a]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0555
  (let [s1 (parse (many (optional letter)) "ABCDEGFHIJK*")]
    (fact "many - skips optional items; consumes input though value is empty"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  (seq "ABCDEGFHIJK")
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0560
  (let [in "a1b3c4d5e7f8"
	p1 (<*> letter digit)
	s1 (parse (many p1) in)]
    (fact "many - parses six compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0560-05
  (let [s1 (parse (many lower) "*")]
    (fact "many - collects the result in a vector; parses zero items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  empty?
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0560-10
  (let [s1 (parse (many lower) "a*")]
    (fact "many - collects the result in a vector; parses one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\a]

	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0565
  (let [s1 (parse (<|> (many lower) (sym* \*)) "*")]
    (fact "many - consumes no input and succeeds; <|> returns its value"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0570
  (let [in "a1b3c4d5ee"
	p1 (<*> letter digit)
	s1 (parse (many p1) in)
	em (get-msg-str (:error s1))]
    (fact "many - parses four compound items, then fails in the next compound item"
	  (:input s1)  =>  [\e]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\e\nexpecting digit")))


(deftest test-0575
  (let [s1 (parse (many1 lower) "*")
	em (get-msg-str (:error s1))]
    (fact "many1 - fails with zero items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting lowercase letter")))


(deftest test-0580
  (let [s1 (parse (many1 lower) "a*")]
    (fact "many1 - parses one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\a]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0580-05
  (let [s1 (parse (many1 lower) "a*")]
    (fact "many1 - collects the result in a vector; parses one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\a]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0585
  (let [in "a1b3c4d5e7f8"
	p1 (<*> letter digit)
	s1 (parse (many1 p1) in)]
    (fact "many1 - parses six compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  '((\a \1) (\b \3) (\c \4) (\d \5) (\e \7) (\f \8))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0590
  (let [s1 (parse (<|> (many1 lower) (sym* \*)) "w*")]
    (fact "many1 - consumes input; <|> returns its value"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\w]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0595
  (let [s1 (parse (<|> (many1 digit) upper) "*")
	em (get-msg-str (:error s1))]
    (fact "many1 - fails; passes on empty, errors to <|>"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit or uppercase letter")))


(deftest test-0600
  (let [s1 (parse (<|> (many1 digit) upper) "A")]
    (fact "many1 - fails; passes on empty, are cleared in <|>"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \A
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-0605
  (let [in "a1b3c4d5ee"
	p1 (<*> letter digit)
	s1 (parse (many1 p1) in)
	em (get-msg-str (:error s1))]
    (fact "many1 - parses four compound items, then fails in the next compound item"
	  (:input s1)  =>  [\e]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\e\nexpecting digit")))


(deftest test-0610
  (let [s1 (parse (optional (<*> upper digit)) "U2*")]
    (fact "optional - parses an optional item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\U \2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0615
  (let [s1 (parse (optional (<*> upper digit)) "u2*")]
    (fact "optional - fails consuming no input"
	  (:input s1)  =>  [\u \2 \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0620
  (let [s1 (parse (optional (<*> upper digit)) "UP*")
	em (get-msg-str (:error s1))]
    (fact "optional - fails consuming input"
	  (:input s1)  =>  [\P \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\P\nexpecting digit")))


(deftest test-0625
  (let [p1 (<$> (partial apply str) (<*> (optional upper) (sym* \*)))
	s1 (parse p1 "U*")]
    (fact "optional - skips the optional char"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "U*"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0630
  (let [s1 (parse (option "XY" (<*> upper digit)) "U2*")]
    (fact "option - parses an item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\U \2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0635
  (let [s1 (parse (option "XY" (<*> upper digit)) "u2*")]
    (fact "option - fails without consuming input; produces optional value"
	  (:input s1)  =>  [\u \2 \*]
	  (:value s1)  =>  "XY"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0640
  (let [s1 (parse (option "XY" (<*> upper digit)) "UP*")
	em (get-msg-str (:error s1))]
    (fact "option - fails consuming input"
	  (:input s1)  =>  [\P \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\P\nexpecting digit")))


(deftest test-0640-05
  (let [s1 (parse (skip (sym* \*)) "*")]
    (fact "skip - skips a star"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0640-10
  (let [s1 (parse (skip letter digit) "U2")]
    (fact "skip - skips a letter and a digit"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0640-15
  (let [s1 (parse (skip (sym* \*) letter digit (sym* \*)) "*U2*")]
    (fact "skip - skips a star, a letter, a digit, and a star"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0645
  (let [s1 (parse (skip-many letter) "*")]
    (fact "skip-many - skips zero letters"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0650
  (let [s1 (parse (skip-many (<*> letter digit)) "A*")
	em (get-msg-str (:error s1))]
    (fact "skip-many - skips zero compound items; <*> fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0655
  (let [s1 (parse (skip-many (<*> digit lower)) "0x*")]
    (fact "skip-many - skips one compound item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0660
  (let [s1 (parse (skip-many letter) "abcdefghijk*")]
    (fact "skip-many - skips letters"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0665
  (let [s1 (parse (skip-many (<*> digit lower)) "0x1y2z*")]
    (fact "skip-many - skips three compound items; consumes no more input"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0670
  (let [s1 (parse (>> (skip-many (optional digit)) (sym* \*)) "0123456789*")]
    (fact "skip-many - skips optional items; then consumes more input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \*
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0675
  (let [s1 (parse (skip-many1 letter) "*")
	em (get-msg-str (:error s1))]
    (fact "skip-many1 - fails with zero letters"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting letter")))


(deftest test-0680
  (let [s1 (parse (skip-many1 (<*> letter digit)) "A*")
	em (get-msg-str (:error s1))]
    (fact "skip-many1 - skips zero compound items; <*> fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0685
  (let [s1 (parse (skip-many1 (<*> digit lower)) "0x*")]
    (fact "skip-many1 - skips one compound item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0690
  (let [s1 (parse (skip-many1 letter) "abcdefghijk*")]
    (fact "skip-many1 - skips letters"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0695
  (let [s1 (parse (skip-many1 (<*> digit lower)) "0x1y2z*")]
    (fact "skip-many1 - skips three compound items; consumes no more input"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0700
  (let [s1 (parse (>> (skip-many1 (optional digit)) (sym* \*)) "0123456789*")]
    (fact "skip-many1 - skips optional items; then consumes more input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  \*
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0705
  (let [s1 (parse (sep-by (sym* \,) digit) "*")]
    (fact "sep-by - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0710
  (let [s1 (parse (sep-by (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "sep-by - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0715
  (let [s1 (parse (sep-by (sym* \,) digit) "0*")]
    (fact "sep-by - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0720
  (let [s1 (parse (sep-by (sym* \,) (<*> upper digit)) "U2*")]
    (fact "sep-by - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0725
  (let [s1 (parse (sep-by (sym* \,) (>> letter digit)) "U2,*")
    	em (get-msg-str (:error s1))]
    (fact "sep-by - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter")))


(deftest test-0730
  (let [s1 (parse (sep-by (sym* \,) digit) "0,1*")]
    (fact "sep-by - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0730-05
  (let [s1 (parse (sep-by (sym* \,) digit) "0,1*")]
    (fact "sep-by - collects the result in a vector; two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0735
  (let [s1 (parse (sep-by (sym* \,) (>> (sym* \+) digit)) "+0,+1*")]
    (fact "sep-by - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0740
  (let [p1 (sep-by (many white-space) (many letter))
	s1 (parse p1 "one two \t\t  three")]
    (fact "sep-by - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "three"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0745
  (let [s1 (parse (sep-by1 (sym* \,) digit) "*")
	em (get-msg-str (:error s1))]
    (fact "sep-by1 - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0750
  (let [s1 (parse (sep-by1 (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "sep-by1 - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0755
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0*")]
    (fact "sep-by1 - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0760
  (let [s1 (parse (sep-by1 (sym* \,) (<*> upper digit)) "U2*")]
    (fact "sep-by1 - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0765
  (let [s1 (parse (sep-by1 (sym* \,) (>> letter digit)) "U2,*")
    	em (get-msg-str (:error s1))]
    (fact "sep-by1 - there is only one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter")))


(deftest test-0770
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0,1*")]
    (fact "sep-by1 - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0770-05
  (let [s1 (parse (sep-by1 (sym* \,) digit) "0,1*")]
    (fact "sep-by1 - collects the result in a vector; two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0775
  (let [s1 (parse (sep-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1*")]
    (fact "sep-by1 - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0780
  (let [p1 (sep-by1 (many white-space) (many letter))
	s1 (parse p1 "one two \t\t  three")]
    (fact "sep-by1 - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "three"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0785
  (let [s1 (parse (sep-by1 (sym* \|) (>> upper digit)) "A1|B2|C3|DD,*")
    	em (get-msg-str (:error s1))]
    (fact "sep-by1 - compound item fails after reading several items"
	  (:input s1)  =>  [\D \, \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\D\nexpecting digit")))


(deftest test-0790
  (let [s1 (parse (end-by (sym* \,) digit) "*")]
    (fact "end-by - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0800
  (let [s1 (parse (end-by (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "end-by - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0805
  (let [s1 (parse (end-by (sym* \,) digit) "0*")
	em (get-msg-str (:error s1))]
    (fact "end-by - one item; with no separator it fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting \\,")))


(deftest test-0810
  (let [s1 (parse (end-by (sym* \,) (<*> upper digit)) "U2*")
	em (get-msg-str (:error s1))]
    (fact "end-by - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting \\,")))


(deftest test-0815
  (let [s1 (parse (end-by (sym* \,) (>> letter digit)) "U2,*")]
    (fact "end-by - there is one item that ends with a separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0820
  (let [s1 (parse (end-by (sym* \,) digit) "0,1,*")]
    (fact "end-by - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0820-05
  (let [s1 (parse (end-by (sym* \,) digit) "0,1,*")]
    (fact "end-by - collects the result in a vector; two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0825
  (let [s1 (parse (end-by (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (fact "end-by - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0830
  (let [p1 (end-by (many white-space) (many letter))
	s1 (parse p1 "one two \t\t  three\n")]
    (fact "end-by - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "three"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0835
  (let [s1 (parse (end-by1 (sym* \,) digit) "*")
	em (get-msg-str (:error s1))]
    (fact "end-by1 - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0840
  (let [s1 (parse (end-by1 (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "end-by1 - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0845
  (let [s1 (parse (end-by1 (sym* \,) digit) "0*")
	em (get-msg-str (:error s1))]
    (fact "end-by1 - one item; with no separator it fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting \\,")))


(deftest test-0850
  (let [s1 (parse (end-by1 (sym* \,) (<*> upper digit)) "U2*")
	em (get-msg-str (:error s1))]
    (fact "end-by1 - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting \\,")))


(deftest test-0855
  (let [s1 (parse (end-by1 (sym* \,) (>> letter digit)) "U2,*")]
    (fact "end-by1 - there is one item and the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
          (:error s1)  =>  nil)))


(deftest test-0860
  (let [s1 (parse (end-by1 (sym* \,) digit) "0,1,*")]
    (fact "end-by1 - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0860-05
  (let [s1 (parse (end-by1 (sym* \,) digit) "0,1,*")]
    (fact "end-by1 - collects the result in a vector; two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0865
  (let [s1 (parse (end-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (fact "end-by1 - two compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0870
  (let [p1 (end-by1 (many white-space) (many letter))
	s1 (parse p1 "one two \t\t  three\n")]
    (fact "end-by1 - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "three"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0880
  (let [s1 (parse (end-by1 (sym* \|) (>> upper digit)) "A1|B2|C3|DD,*")
    	em (get-msg-str (:error s1))]
    (fact "end-by1 - compound item fails after reading several items"
	  (:input s1)  =>  [\D \, \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\D\nexpecting digit")))


(deftest test-0885
  (let [s1 (parse (sep-end-by (sym* \,) digit) "*")]
    (fact "sep-end-by - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-0890
  (let [s1 (parse (sep-end-by (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "sep-end-by - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0895
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0*")]
    (fact "sep-end-by - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0900
  (let [s1 (parse (sep-by (sym* \,) (<*> upper digit)) "U2*")]
    (fact "sep-by - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0905
  (let [s1 (parse (sep-end-by (sym* \,) (>> letter digit)) "U2,*")]
    (fact "sep-end-by - one item ended by the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0910
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0,1*")]
    (fact "sep-end-by - two simple items separated by ,"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0910-05
  (let [s1 (parse (sep-end-by (sym* \,) digit) "0,1*")]
    (fact "sep-end-by - collects the result in a vector; two simple items separated by ,"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))

(deftest test-0915
  (let [s1 (parse (sep-end-by (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (fact "sep-end-by - two compound items separated and ended by ,"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0920
  (let [p1 (sep-end-by (many1 white-space) (<*> letter letter letter))
	s1 (parse p1 "one two\t \tsix\n")]
    (fact "sep-end-by - three compound items; using many1 to avoid an SO"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "six"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0925
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "*")
	em (get-msg-str (:error s1))]
    (fact "sep-end-by1 - there are no separated items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0930
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> letter digit)) "A*")
    	em (get-msg-str (:error s1))]
    (fact "sep-end-by1 - there are no separated compound items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting digit")))


(deftest test-0935
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "0*")]
    (fact "sep-end-by1 - one item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0940
  (let [s1 (parse (sep-end-by1 (sym* \,) (<*> upper digit)) "U2*")]
    (fact "sep-end-by1 - one compound item, no separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [[\U \2]]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0950
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> letter digit)) "U2,*")]
    (fact "sep-end-by1 - one compound item ended by the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0960
  (let [s1 (parse (sep-end-by1 (sym* \,) digit) "0,1*")]
    (fact "sep-end-by1 - two simple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0965
  (let [s1 (parse (sep-end-by1 (sym* \,) (>> (sym* \+) digit)) "+0,+1,*")]
    (fact "sep-end-by1 - two compound items ended by the separator"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\0 \1]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0970
  (let [p1 (sep-end-by1 white-space (<*> letter letter letter))
	s1 (parse p1 "one two\tsix\n")]
    (fact "sep-end-by1 - three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "six"))
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0970-05
  (let [p1 (sep-end-by1 white-space (<*> letter letter letter))
	s1 (parse p1 "one two\tsix\n")]
    (fact "sep-end-by1 - collects the result in a vector; three compound items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (list (seq "one") (seq "two") (seq "six"))
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0970-10
  (let [p1 (sep-end-by1 white-space (many1 letter))
	s1 (parse p1 "one")]
    (fact "sep-end-by1 - collects the result in a vector; one compound item"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [(seq "one")]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0975
  (let [s1 (parse (sep-end-by1 (sym* \|) (>> upper digit)) "A1|B2|C3|DD,*")
    	em (get-msg-str (:error s1))]
    (fact "sep-end-by1 - compound item fails after reading several items"
	  (:input s1)  =>  [\D \, \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\D\nexpecting digit")))


(deftest test-0980
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "{0}*")]
    (fact "between - one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  \0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0980-05
  (let [s1 (parse (between (sym* \:) digit) ":0:*")]
    (fact "between - with same delimiter - one item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  \0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0980-10
  (let [s1 (parse (between (sym* \|) (many digit)) "|5005|*")]
    (fact "between - with same delimiter - multiple items"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  [\5 \0 \0 \5]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0985
  (let [p1 (>>= letter
		(fn [x] (>>= (many alpha-num)
			     (fn [y] (return (cons x y))))))
	s1 (parse (between (sym* \{) (sym* \}) p1) "{abc101z}*")]
    (fact "between - one compound item"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  (seq "abc101z")
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-0990
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "(0}*")
    	em (get-msg-str (:error s1))]
    (fact "between - the open parser fails"
	  (:input s1)  =>  [\( \0 \} \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\(\nexpecting \\{")))


(deftest test-0995
  (let [s1 (parse (between (sym* \{) (sym* \}) digit) "{0)*")
    	em (get-msg-str (:error s1))]
    (fact "between - the close parser fails"
	  (:input s1)  =>  [\) \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\)\nexpecting \\}")))


(deftest test-1000
  (let [s1 (parse (times 0 digit) "0*")]
    (fact "times - zero items"
	  (:input s1)  =>  [\0 \*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1001
  (let [s1 (parse (times 1 letter) "x")]
    (fact "times - one item"
	  (:input s1)  =>  []
	  (:value s1)  =>  [\x]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1005
  (let [s1 (parse (times 3 (>> any-char letter)) "*a@b$c")]
    (fact "times - three items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\a \b \c]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1005-05
  (let [s1 (parse (times 3 (>> any-char letter)) "*a@b$c")]
    (fact "times - collects the result in a vector; three items"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\a \b \c]
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1010
  (let [s1 (parse (times 3 (>> any-char letter)) "*a@b$$")
    	em (get-msg-str (:error s1))]
    (fact "times - two items, then fails"
	  (:input s1)  =>  [\$]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\$\nexpecting letter")))


(deftest test-1015
  (let [s1 (parse (<$> count (look-ahead (many digit))) "12345678")]
    (fact "look-ahead - succeeds consuming input"
	  (:input s1)  =>  (seq "12345678")
	  (:value s1)  =>  8
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1020
  (let [s1 (parse (look-ahead (many digit)) "YYZ")]
    (fact "look-ahead - succeeds consuming no input"
	  (:input s1)  =>  [\Y \Y \Z]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1025
  (let [s1 (parse (look-ahead digit) "*")]
    (fact "look-ahead - fails consuming no input"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1030
  (let [s1 (parse (look-ahead (>> letter digit)) "A*")]
    (fact "look-ahead - fails consuming no input"
	  (:input s1)  =>  [\A \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1030-05
  (let [s1 (parse (predict (sym* \=)) "=10")]
    (fact "predict - if p succeeds it consumes no input"
	  (:input s1)  =>  [\= \1 \0]
	  (:value s1)  =>  \=
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1030-10
  (let [s1 (parse (predict (sym* \=)) "<10")]
    (fact "predict - if p succeeds it consumes no input"
	  (:input s1)  =>  [\< \1 \0]
	  (:value s1)  =>  nil?
	  (:ok    s1)  =>  false?
	  (:empty s1)  =>  true?)))


(deftest test-1035
  (let [s1 (parse (not-followed-by digit) "**")]
    (fact "not-followed-by - succeeds as digit fails"
	  (:input s1)  =>  [\* \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1040
  (let [s1 (parse (not-followed-by (>> letter letter digit)) "xy**")]
    (fact "not-followed-by - succeeds as compound item fails"
	  (:input s1)  =>  [\x \y \* \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1045
  (let [s1 (parse (not-followed-by (<*> upper digit)) "U2*")
    	em (get-msg-str (:error s1))]
    (fact "not-followed-by - fails as the parse succeeds"
	  (:input s1)  =>  [\U \2 \*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected [\\U \\2]")))


(deftest test-1050
  (let [s1 (parse (>> upper upper upper eof) "YYZ")]
    (fact "eof - there's nothing left"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1055
  (let [s1 (parse eof "")]
    (fact "eof - there's nothing to begin with"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  true
	  (:error s1)  =>  nil)))


(deftest test-1060
  (let [s1 (parse (<*> upper digit eof) "U2*")
    	em (get-msg-str (:error s1))]
    (fact "eof - fails because the input isn't empty"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected *\nexpecting end of input")))


(deftest test-1065
  (let [s1 (parse (many-till digit letter) "123456A")]
    (fact "many-till - parses several numbers, then a letter"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq "123456")
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1065-05
  (let [s1 (parse (many-till digit letter) "123456A")]
    (fact "many-till - collects the result in a vector; several numbers and a letter"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq "123456")
	  (:value s1)  =>  vector?
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1070
  (let [s1 (parse (many-till digit letter) "A*")]
    (fact "many-till - just the end parser"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  []
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1075
  (let [p1 (>> (token* "<!--") (many-till any-char (<:> (token* "-->"))))
	s1 (parse p1 "<!-- -->")]
    (fact "many-till - reads a space between xml comments"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [\space]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1080
  (let [p1 (>> (token* "<!--") (many-till any-char (<:> (token* "-->"))))
	s1 (parse p1 "<!--foobar-->")]
    (fact "many-till - reads a word between xml comments"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  (seq "foobar")
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false
	  (:error s1)  =>  nil)))


(deftest test-1085
  (let [s1 (parse (many-till digit letter) "*A")
    	em (get-msg-str (:error s1))]
    (fact "many-till - fails parsing the prefix"
	  (:input s1)  =>  [\* \A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true
	           em  => "unexpected \\*\nexpecting letter or digit")))


(deftest test-1090
  (let [s1 (parse (many-till digit letter) "12345*A")
    	em (get-msg-str (:error s1))]
    (fact "many-till - parses several prefixes and then fails"
	  (:input s1)  =>  [\* \A]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting letter or digit")))


(deftest test-1095
  (let [s1 (parse (many-till digit (>> upper (sym* \X))) "12345A*")
    	em (get-msg-str (:error s1))]
    (fact "many-till - parses the prefix, then fails reading the end parser"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	           em  => "unexpected \\*\nexpecting \\X")))


(deftest test-1113
  (let [s1 (parse (<+> letter) "a")]
    (fact "<+> - cats from one parser"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "a"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1114
  (let [s1 (parse (<+> (times 3 (sym* \S))) "SSS")]
    (fact "<+> - cats from one parser"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "SSS"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1115
  (let [s1 (parse (<+> (sym* \-) digit) "-1")]
    (fact "<+> - cats from two parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "-1"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1120
  (let [s1 (parse (<+> (sym* \-) digit (sym* \;)) "-1;")]
    (fact "<+> - cats from three parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "-1;"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1125
  (let [p1 (<+> letter (>> (sym* \|) letter) (>> (sym* \|) digit))
	s1 (parse  p1 "X|Y|9")]
    (fact "<+> - cats from filtering parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "XY9"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1130
  (let [in "ABC012"
	p1 (<+> (<*> letter letter letter) (<*> digit digit digit))
	s1 (parse  p1 in)]
    (fact "<+> - cats from compound parsers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  "ABC012"
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1135
  (let [s1 (parse (<+> letter digit) "*")]
    (fact "<+> - the first parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  true)))


(deftest test-1140
  (let [s1 (parse (<+> letter digit) "A*")]
    (fact "<+> - the second parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-1145
  (let [s1 (parse (<+> letter tab digit) "A\t*")]
    (fact "<+> - the third parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-1150
  (let [p1 (<+> letter tab tab tab tab tab (sym* \x))
	s1 (parse p1 "A\t\t\t\t\t*")]
    (fact "<+> - the seventh parser fails"
	  (:input s1)  =>  [\*]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false)))


(deftest test-1150-05
  (let [s1 (parse (search dec-num) "Now I have 20 dollars")]
    (fact "search - a simple number"
	  (:input s1)  =>  (seq " dollars")
	  (:value s1)  =>  20
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1150-10
  (let [s1 (parse (many (search dec-num)) "Now I have 20 dollars, or 2 tens")]
    (fact "search - multiple numbers"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [20 2]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1150-15
  (let [s1 (parse (many (search (<|> dec-num (token* "dollars")))) "Now I have 20 dollars")]
    (fact "search - multiple choices, multiple times"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  [20 "dollars"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


;; +-------------------------------------------------------------+
;; |                        Parser state.                        |
;; +-------------------------------------------------------------+


(deftest test-1155
  (let [p1 (>> (put-state 0)
               (skip-many 
                 (bind [x any-char]
		   (if (= x \newline) (modify-state inc) (return nil)))))
	s1 (parse p1 "aaa\nbbb\nccc\nddd\nfff\nggg\nhhh\n\niii\njjj\n\nkkk")]
    (fact "put-state, modify-state"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  true
	  (:user  s1)  =>  11
	  (:empty s1)  =>  false)))


(deftest test-1160
  (let [p1 (>> (skip (put-state 0)
                     (skip-many 
                       (bind [x any-char]
                          (if (= x \newline) (modify-state inc) (return nil)))))
               get-state)
	s1 (parse p1 "aaa\nbbb\nccc\nddd\nfff\nggg\nhhh\n\niii\njjj\n\nkkk")]
    (fact "put-state, get-state; get user state as the parser's value"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  11
	  (:ok    s1)  =>  true
	  (:user  s1)  =>  11
	  (:empty s1)  =>  false)))


(deftest test-1165
  (let [in "ABC"
	p1 (<+> letter letter letter)
	p2 (>> (set-input "XYZ") p1)
	s1 (parse (<*> p1 p2) in)]
    (fact "set-input"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  ["ABC" "XYZ"]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1170
  (let [in "ABC"
	p1 (<+> letter letter letter)
	p2 (>> (skip (set-input "XY0")
		     (set-position (make-pos "include"))) p1)
	s1 (parse (<*> p1 p2) in)
    	em (get-msg-str (:error s1))
	ip (:pos s1)]
    (fact "set-input, set-position"
	  (:input s1)  =>  [\0]
	  (:value s1)  =>  nil
	  (:ok    s1)  =>  false
	  (:empty s1)  =>  false
	  (:src   ip)  =>  "include"
	  (:line  ip)  =>  1
	  (:col   ip)  =>  3
	           em  =>  "unexpected \\0\nexpecting letter")))


(deftest test-1175
  (let [in "ABC"
	p1 (<+> letter letter letter)
	p2 (>> (skip (set-input "WXYZ")
		     (set-position (make-pos "include"))) get-position)
	s1 (parse (>> p1 p2) in)
	v1 (:value s1)]
    (fact "set-input, set-position, get-position"
	  (:input s1)  =>  (seq "WXYZ")
	  (:src   v1)  =>  "include"
	  (:line  v1)  =>  1
	  (:col   v1)  =>  1
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1180
  (let [in "ABC"
	p1 (<+> letter letter letter)
	p2 (>> (set-input "XYZ") get-input)
	s1 (parse (>> p1 p2) in)]
    (fact "set-input, get-input"
	  (:input s1)  =>  [\X \Y \Z]
	  (:value s1)  =>  [\X \Y \Z]
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


;; +-------------------------------------------------------------+
;; |                      Numeric parsers.                       |
;; +-------------------------------------------------------------+


(deftest test-1185
  (let [s1 (parse dec-num "747")]
    (fact "dec-num - reads a simple integer"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1190
  (let [s1 (parse dec-num "747-600")]
    (fact "dec-num - reads a simple integer, delimited"
	  (:input s1)  =>  [\- \6 \0 \0]
	  (:value s1)  =>  747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1195
  (let [s1 (parse (>> upper dec-num) "A380aircraft")]
    (fact "dec-num - reads an integer, delimited"
	  (:input s1)  =>  (seq "aircraft")
	  (:value s1)  =>  380
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1200
  (let [s1 (parse dec-num "987654321987654321000|")]
    (fact "dec-num - reads an integer, delimited"
	  (:input s1)  =>  [\|]
	  (:value s1)  =>  987654321987654321000N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1205
  (let [s1 (parse oct-num "0747")]
    (fact "oct-num - reads a simple octal number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1210
  (let [s1 (parse oct-num "0747-600")]
    (fact "oct-num - reads a simple octal number, delimited"
	  (:input s1)  =>  [\- \6 \0 \0]
	  (:value s1)  =>  0747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1215
  (let [s1 (parse (>> upper oct-num) "B767aircraft")]
    (fact "oct-num - reads an octal number, delimited"
	  (:input s1)  =>  (seq "aircraft")
	  (:value s1)  =>  0767
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1220
  (let [s1 (parse oct-num "76543217654321000000|")]
    (fact "oct-num - reads an octal number, delimited"
	  (:input s1)  =>  [\|]
	  (:value s1)  =>  076543217654321000000N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1225
  (let [s1 (parse hex-num "747")]
    (fact "hex-num - reads a simple hex number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  0x747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1230
  (let [s1 (parse hex-num "747-600")]
    (fact "hex-num - reads a simple hex number, delimited"
	  (:input s1)  =>  [\- \6 \0 \0]
	  (:value s1)  =>  0x747
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1235
  (let [s1 (parse (>> upper hex-num) "A380plane")]
    (fact "hex-num - reads a hex number, delimited"
	  (:input s1)  =>  (seq "plane")
	  (:value s1)  =>  0x380
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1240
  (let [s1 (parse hex-num "ABCDEF987654321987654321000|")]
    (fact "hex-num - reads a hex number, delimited"
	  (:input s1)  =>  [\|]
	  (:value s1)  =>  0xABCDEF987654321987654321000N
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1245
  (let [s1 (parse float-num "100")]
    (fact "float-num - reads a simple floating-point number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  100.0
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1250
  (let [s1 (parse float-num "3.1415927")]
    (fact "float-num - reads a simple floating-point number"
	  (:input s1)  =>  empty?
	  (:value s1)  =>  3.1415927
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1255
  (let [s1 (parse float-num "9.8m/s")]
    (fact "float-num - reads a simple floating-point number, delimited"
	  (:input s1)  =>  [\m \/ \s]
	  (:value s1)  =>  9.8
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1260
  (let [s1 (parse float-num "0.00343ms")]
    (fact "float-num - reads a floating-point number, delimited"
	  (:input s1)  =>  [\m \s]
	  (:value s1)  =>  0.00343
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))


(deftest test-1265
  (let [s1 (parse float-num "98765432.19876543555666|")]
    (fact "float-num - reads a floating-point number, delimited"
	  (:input s1)  =>  [\|]
	  (:value s1)  =>  9.876543219876544E7
	  (:ok    s1)  =>  true
	  (:empty s1)  =>  false)))
