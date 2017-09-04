;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc    "The Kern Lexer library."
      :author "Armando Blancas"}
blancas.kern.lexer
  (:require [blancas.kern.core :refer [<|> <?> >> >>= <:> <+> << <*>] :as k]
            [blancas.kern.i18n :refer [i18n fmt]]
            [clojure.string :refer [lower-case]])
  #?(:cljs (:require-macros [blancas.kern.core :as k])))


;; +-------------------------------------------------------------+
;; |                    Language definitions.                    |
;; +-------------------------------------------------------------+


;; A language def record customizes the parsers returned by make-parsers.
(defrecord LanguageDef
  [type                                                     ;; Identifies the type of settings.
   comment-start                                            ;; A string that marks the start of a block comment.
   comment-end                                              ;; A string that marks the end of a block comment.
   comment-line                                             ;; A string that marks the start of a line comment.
   nested-comments                                          ;; Whether the lexer accepts nested comments; a boolean.
   identifier-start                                         ;; A parser for the start of an identifier.
   identifier-letter                                        ;; A parser for the subsequent characters of an identifier.
   reserved-names                                           ;; A list of names that cannot be identifiers.
   case-sensitive                                           ;; Whether tokens are case-sensitive; a boolean.
   line-continuation                                        ;; A parser for the token that precedes the new line.
   trim-newline                                             ;; Treats newline character(s) as whitespace.
   leading-sign])                                           ;; Whether numbers accept an optional leading sign.


(def basic-def
  "The most basic record; for use to build new styles."
  (map->LanguageDef
    {:type              :basic
     :comment-start     ""
     :comment-end       ""
     :comment-line      ""
     :nested-comments   false
     :identifier-start  (<|> k/letter (k/sym* \_))
     :identifier-letter (<|> k/alpha-num (k/sym* \_))
     :reserved-names    []
     :case-sensitive    true
     :line-continuation (k/sym* \\)
     :trim-newline      true
     :leading-sign      true}))


(def haskell-style-def
  "Lexical settings for Haskell-style languages."
  (assoc basic-def
    :type :Haskell
    :comment-start "{-"
    :comment-end "-}"
    :comment-line "--"
    :nested-comments true))


(def java-style-def
  "Lexical settings for Java-style languages."
  (assoc basic-def
    :type :Java
    :comment-start "/*"
    :comment-end "*/"
    :comment-line "//"))


(def c-style-def
  "Lexical settings for C-style languages."
  (assoc java-style-def
    :type :C))


(def shell-style-def
  "Lexical settings for shell-style languages."
  (assoc basic-def
    :type :Shell
    :comment-line "#"
    :identifier-letter (<|> k/alpha-num (k/one-of* "_-."))
    :trim-newline false))


;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+


(defrecord TokenParsers
  [trim
   lexeme
   sym
   new-line
   one-of
   none-of
   token
   word
   identifier
   field
   char-lit
   string-lit
   dec-lit
   oct-lit
   hex-lit
   float-lit
   bool-lit
   nil-lit
   parens
   braces
   angles
   brackets
   semi
   comma
   colon
   dot
   semi-sep
   semi-sep1
   comma-sep
   comma-sep1])


(k/defn* trim
         "Skips over any whitespace, including comments (if defined), at
          the start of the input. Whether newline characters are removed
          as whitespace is configured by :trim-newline. When that setting
          is true, the setting :line-continuation is activated."
         [] nil)

(k/defn* lexeme
         "Applies parser p, then calls (trim)."
         [p] nil)

(k/defn* sym
         "Parses a single character c. Compares according to
          :case-sensitive. Calls (trim) afterwards."
         [c] nil)

(k/defn* new-line
         "Parses a new line, UNIX or Windows style; then calls (trim)."
         [] nil)

(k/defn* one-of
         "Succeeds if the next character is in the supplied string.
          Calls (trim) afterwards."
         [cs] nil)

(k/defn* none-of
         "Succeeds if the next character is not in the supplied string.
          Calls (trim) afterwards."
         [] nil)

(k/defn* token
         "Parses a specific string, not necessarily delimited. If more
          than one are given it will try each choice in turn. Compares
          according to :case-sensitive. Calls (trim) afterwards."
         ([cs] nil)
         ([cs & more] nil))

(k/defn* word
         "Parses a specific string; must be delimited by any character not
          parsed by :identifier-letter. If more than one are given it will
          try each choice in turn. Compares according to :case-sensitive.
          Calls (trim) afterwards."
         ([cs] nil)
         ([cs & more] nil))

(k/defn* identifier
         "Parses an unquoted string suitable for an identifier or a name.
          The start of the input is defined by :identifier-start, and
          subsequent symbols by :identtifier-letter. It will check that
          the parsed value not be in the list of :reserved-names, if any,
          comparing according to :case-sensitive. Calls (trim) afterwards."
         [] nil)

(k/defn* field
         "Parses an unquoted text field terminated by any character
          in cs. Calls (trim) afterwards."
         [cs] nil)

(k/defn* char-lit
         "Parses a character literal according to the :type setting. The
          common syntax is a symbol in single quotes with the usual
          escape codes. Calls (trim) afterwards.

          The following styles add escaped characters:

          :basic   \\b \\t \\n \\f \\r \\' \\\" \\/
          :C       :basic + \\0ooo \\0xnn \\unnnnnnnn
          :Haskell :basic + \\nnnn \\onnnn \\xnnnn
          :Java    :basic + \\0ooo \\unnnn
          :Shell   :basic + \\0ooo \\0xnn \\unnnnnnnn"
         [] nil)

(k/defn* string-lit
         "Parses a string literal according to the :type setting. The
          common syntax is any number of symbols in double quotes
          with the usual escape codes. Calls (trim) afterward.

          The following styles add escaped characters:

          :basic   \\b \\t \\n \\f \\r \\' \\\" \\/
          :C       :basic + \\0ooo \\0xnn \\unnnnnnnn
          :Haskell :basic + \\nnnn \\onnnn \\xnnnn
          :Java    :basic + \\0ooo \\unnnn
          :Shell   :basic + \\0ooo \\0xnn \\unnnnnnnn"
         [] nil)

(k/defn* dec-lit
         "Parses a decimal number as Long or BigInt depending on the
          magnitude or if it ends with N. Calls (trim) afterward."
         [] nil)

(k/defn* oct-lit
         "Parses an octal number as Long or BigInt depending on the
          magnitude or if it ends with N. Calls (trim) afterward."
         [] nil)

(k/defn* hex-lit
         "Parses a hexadecimal number as Long or BigInt depending on the
          magnitude or if it ends with N. Calls (trim) afterward."
         [] nil)

(k/defn* float-lit
         "Parses a floating-point number as Double or BigDecimal depending
          on the magnitude or if it ends with M. It cannot start with a
          period. The first period found must be followed by at least one
          digit. Calls (trim) afterward."
         [] nil)

(k/defn* bool-lit
         "Parses a boolean value, true or false, comparing according to
          :case-sensitive. Calls (trim) afterward."
         [] nil)

(k/defn* nil-lit
         "Parses a null value, nil or null, comparing according to
          :case-sensitive. Calls (trim) afterward."
         [] nil)

(k/defn* parens
         "Applies parser p skiping over surrounding parenthesis.
          Calls (trim) after the opening paren, after p, and after
          the closing paren."
         [p] nil)

(k/defn* braces
         "Applies parser p skiping over surrounding braces.
          Calls (trim) after the opening brace, after p, and after
          the closing brace."
         [p] nil)

(k/defn* angles
         "Applies parser p skiping over surrounding angle brackets.
          Calls (trim) after the opening bracket, after p, and after
          the closing bracket."
         [p] nil)

(k/defn* brackets
         "Applies parser p skiping over surrounding brackets.
          Calls (trim) after the opening bracket, after p, and after
          the closing bracket."
         [p] nil)

(k/defn* semi
         "Parses a single semicolon; then calls (trim)."
         [] nil)

(k/defn* comma
         "Parses a single comma; then calls (trim)."
         [] nil)

(k/defn* colon
         "Parses a single colon; then calls (trim)."
         [] nil)

(k/defn* dot
         "Parses a single dot; then calls (trim)."
         [] nil)

(k/defn* semi-sep
         "Applies parser p zero or more times, skiping over separating
          semicolons. Calls (trim) after each p and semicolon."
         [] nil)

(k/defn* semi-sep1
         "Applies parser p one or more times, skiping over separating
          semicolons. Calls (trim) after each p and semicolon."
         [] nil)

(k/defn* comma-sep
         "Applies parser p zero or more times, skiping over separating
          commas. Calls (trim) after each p and comma."
         [] nil)

(k/defn* comma-sep1
         "Applies parser p one or more times, skiping over separating
          commas. Calls (trim) after each p and comma."
         [] nil)


#?(:clj
   (defmacro with-parsers
          "Binds the parser vars in the kern.lexer namespace to the values in rec."
          [rec & body]
          (list 'binding
                ['blancas.kern.lexer/trim (list :trim rec)
                 'blancas.kern.lexer/lexeme (list :lexeme rec)
                 'blancas.kern.lexer/sym (list :sym rec)
                 'blancas.kern.lexer/new-line (list :new-line rec)
                 'blancas.kern.lexer/one-of (list :one-of rec)
                 'blancas.kern.lexer/none-of (list :none-of rec)
                 'blancas.kern.lexer/token (list :token rec)
                 'blancas.kern.lexer/word (list :word rec)
                 'blancas.kern.lexer/identifier (list :identifier rec)
                 'blancas.kern.lexer/field (list :field rec)
                 'blancas.kern.lexer/char-lit (list :char-lit rec)
                 'blancas.kern.lexer/string-lit (list :string-lit rec)
                 'blancas.kern.lexer/dec-lit (list :dec-lit rec)
                 'blancas.kern.lexer/oct-lit (list :oct-lit rec)
                 'blancas.kern.lexer/hex-lit (list :hex-lit rec)
                 'blancas.kern.lexer/float-lit (list :float-lit rec)
                 'blancas.kern.lexer/bool-lit (list :bool-lit rec)
                 'blancas.kern.lexer/nil-lit (list :nil-lit rec)
                 'blancas.kern.lexer/parens (list :parens rec)
                 'blancas.kern.lexer/braces (list :braces rec)
                 'blancas.kern.lexer/angles (list :angles rec)
                 'blancas.kern.lexer/brackets (list :brackets rec)
                 'blancas.kern.lexer/semi (list :semi rec)
                 'blancas.kern.lexer/comma (list :comma rec)
                 'blancas.kern.lexer/colon (list :colon rec)
                 'blancas.kern.lexer/dot (list :dot rec)
                 'blancas.kern.lexer/semi-sep (list :semi-sep rec)
                 'blancas.kern.lexer/semi-sep1 (list :semi-sep1 rec)
                 'blancas.kern.lexer/comma-sep (list :comma-sep rec)
                 'blancas.kern.lexer/comma-sep1 (list :comma-sep1 rec)]
                (cons 'do body))))


;; +-------------------------------------------------------------+
;; |              Private functions for whitespace.              |
;; +-------------------------------------------------------------+


(defn line-comment
  "Parses a line comment."
  [rec]
  (let [start (:comment-line rec)]
    (>>= (k/token* start)
         (fn [_] (>>= (k/many-till k/any-char (<|> k/new-line* k/eof))
                      (fn [_] (k/return nil)))))))


(defn block-nested
  "Parses the contents and end of a nested block comment."
  [rec]
  (let [start (:comment-start rec)
        end   (:comment-end rec)]
    (k/expect (k/many-till (<|> (>>= (<:> (k/token* start)) (fn [_] (block-nested rec)))
                                k/any-char)
                           (k/token* end))
              (i18n :end-comment))))


(defn block-rest
  "Parses the contents and end of a block comment."
  [rec]
  (let [end (:comment-end rec)]
    (k/expect (k/many-till k/any-char (k/token* end)) (i18n :end-comment))))


(defn block-comment
  "Parses a block comment."
  [rec]
  (let [start (:comment-start rec)
        nest? (:nested-comments rec)]
    (k/skip (<:> (k/token* start))
          (if nest? (block-nested rec) (block-rest rec)))))


;; +-------------------------------------------------------------+
;; |    Private functions for character and string literals.     |
;; +-------------------------------------------------------------+


(def space-ascii 32)


(def esc-char
      "Parses an escape code for a basic char."
      (let [codes (zipmap "btnfr'\"\\/" "\b\t\n\f\r'\"\\/")]
        (>>= (<?> (k/one-of* "btnfr'\"\\/") (i18n :esc-code))
             (fn [x] (k/return (get codes x))))))

(defn char-code [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))


(defn basic-char
  "Parses an unquoted character literal. Character c must be escaped."
  [c]
  (<?> (<|> (k/satisfy #(and (not= % c) (not= % \\) (>= (char-code %) space-ascii)))
            (>> (k/sym* \\) esc-char))
       (i18n :char-lit)))


(def esc-oct
      "Parses an octal escape code; the result is the encoded char."
      (>>= (<+> (k/many1 k/oct-digit))
           (fn [x]
             (let [n #?(:clj (Integer/parseInt x 8) :cljs (js/parseInt x 8))]
               (if (<= n 0377)
                 (k/return (char n))
                 (k/fail (i18n :bad-octal)))))))


(def esc-uni
      "Parses a unicode escape code; the result is the encoded char."
      (>>= (<+> (>> (k/sym* \u) (k/times 4 k/hex-digit)))
           (fn [x] (k/return (aget #?(:clj (Character/toChars (Integer/parseInt x 16))
                                      :cljs (.fromCodePoint js/String (js/parseInt x 16))) 0)))))


(defn java-char
  "Parses an unquoted Java character literal. Character c must be escaped."
  [c]
  (<?> (<|> (k/satisfy #(and (not= % c) (not= % \\) (>= (char-code %) space-ascii)))
            (>> (k/sym* \\)
                (<?> (<|> esc-char esc-oct esc-uni)
                     (i18n :esc-code-j))))
       (i18n :char-lit)))


(def c-esc-char
      "Parses an escape code for a C char."
      (let [codes (assoc (zipmap "btnfr'\"\\?/" "\b\t\n\f\r'\"\\?/")
                    \a (char 7) \v (char 11) \0 (char 0))]
        (>>= (k/one-of* "btnfr'\"\\?/av0") (fn [x] (k/return (get codes x))))))


(def c-esc-uni
      "Parses a long unicode escape code; the result is the encoded char."
      (>>= (<+> (>> (k/sym* \U) (k/times 8 k/hex-digit)))
           (fn [x] (k/return (aget #?(:clj (Character/toChars (Integer/parseInt x 16))
                                      :cljs (.fromCodePoint js/String (js/parseInt x 16))) 0)))))


(def c-esc-hex
      "Parses a hex escape code; the result is the encoded char."
      (>>= (<+> (>> (k/sym- \x) (k/times 2 k/hex-digit)))
           (fn [x] (k/return (aget #?(:clj (Character/toChars (Integer/parseInt x 16))
                                      :cljs (.fromCodePoint js/String (js/parseInt x 16))) 0)))))


(defn c-char
  "Parses an unquoted C character literal. Character c must be escaped."
  [c]
  (<?> (<|> (k/satisfy #(and (not= % c) (not= % \\) (>= (char-code %) space-ascii)))
            (>> (k/sym* \\)
                (<?> (<|> c-esc-hex c-esc-char esc-oct esc-uni c-esc-uni)
                     (i18n :esc-code-c))))
       (i18n :char-lit)))


(def h-esc-oct
      "Parses a Haskell octal escape code; the result is the encoded char."
      (>>= (<+> (>> (k/sym* \o) (k/many1 k/oct-digit)))
           (fn [x]
             (let [n #?(:clj (Integer/parseInt x 8) :cljs (js/parseInt x 8))]
               (if (<= n 04177777)
                 (k/return (char n))
                 (k/fail (i18n :bad-oct-h)))))))


(def h-esc-dec
      "Parses a Haskell decimal escape code; the result is the encoded char."
      (>>= (<+> (k/many1 k/digit))
           (fn [x]
             (let [n #?(:clj (Integer/parseInt x) :cljs (js/parseInt x))]
               (if (<= n 1114111)
                 (k/return (char n))
                 (k/fail (i18n :bad-dec-h)))))))


(def h-esc-hex
      "Parses a Haskell hex escape code; the result is the encoded char."
      (>>= (<+> (>> (k/sym* \x) (k/many1 k/hex-digit)))
           (fn [x]
             (let [n #?(:clj (Integer/parseInt x 16) :cljs (js/parseInt x 16))]
               (if (<= n 0x10ffff)
                 (k/return (char n))
                 (k/fail (i18n :bad-hex-h)))))))


(defn haskell-char
  "Parses Haskell character literals."
  [c]
  (<?> (<|> (k/satisfy #(and (not= % c) (not= % \\) (>= (char-code %) space-ascii)))
            (>> (k/sym* \\)
                (<?> (<|> h-esc-hex h-esc-oct c-esc-char h-esc-dec)
                     (i18n :esc-code-h))))
       (i18n :char-lit)))


(defn char-parser
  "Parses character literals delimited by single quotes."
  [lex f]
  (<?> (lex (k/between (k/sym* \') (<?> (k/sym* \') (i18n :end-char)) (f \')))
       (i18n :char-lit)))


(defn str-parser
  "Parses string literals delimited by double quotes."
  [lex f]
  (<?> (lex (k/between (k/sym* \")
                     (<?> (k/sym* \") (i18n :end-string))
                     (<+> (k/many (f \")))))
       (i18n :string-lit)))


;; +-------------------------------------------------------------+
;; |          Private functions for numeric literals.            |
;; +-------------------------------------------------------------+


(def sign (k/optional (k/one-of* "+-")))

(def int-suffix (<|> (<< #?(:clj (k/sym* \N) :cljs (k/skip (k/sym* \N))) (k/not-followed-by k/letter))
                      (k/not-followed-by (<|> k/letter (k/sym* \.)))))

(def float-suffix (<< (k/optional #?(:clj (k/sym* \M) :cljs (k/skip (k/sym* \M)))) (k/not-followed-by k/letter)))


;; +-------------------------------------------------------------+
;; |                      Parser generator.                      |
;; +-------------------------------------------------------------+

(defn read-num-lit [x]
  #?(:clj (read-string x)
     :cljs (js/eval x)))

(defn make-parsers
  "Returns a function map that corresponds to the customization
   values of the input record, whose fields are as follows:

   :type                Identifies the type of settings.
   :comment-start       A string that marks the start of a block comment.
   :comment-end         A string that marks the end of a block comment.
   :comment-line        A string that marks the start of a line comment.
   :nested-comments     Whether the lexer accepts nested comments; a boolean.
   :identifier-start    A parser for the start of an identifier.
   :identifier-letter   A parser for the subsequent characters of an identifier.
   :reserved-names      A list of names that cannot be identifiers.
   :case-sensitive      Whether tokens are case-sensitive; a boolean.
   :line-continuation   A parser for the token that precedes the new line.
   :trim-newline        Treats newline character(s) as whitespace.
   :leading-sign        Whether numbers accept an optional leading sign."
  [rec]
  (let [trim
        (let [line?   (seq (:comment-line rec))
              multi?  (seq (:comment-start rec))
              both?   (and line? multi?)
              ws      (if (:trim-newline rec)
                        k/white-space
                        (<|> (k/one-of* "\t\f\r ")
                             (k/skip (:line-continuation rec) k/new-line*)))
              many-ws (k/skip-many1 ws)]
          (cond both? (k/skip-many (<|> many-ws (line-comment rec) (block-comment rec)))
                line? (k/skip-many (<|> many-ws (line-comment rec)))
                multi? (k/skip-many (<|> many-ws (block-comment rec)))
                :else (k/skip-many ws)))

        lexeme
        (fn [p] (<< p trim))

        sym
        (if (:case-sensitive rec)
          (fn [x] (lexeme (k/sym* x)))
          (fn [x] (lexeme (k/sym- x))))

        new-line
        (lexeme k/new-line*)

        one-of
        (fn [cs] (lexeme (k/one-of* cs)))

        none-of
        (fn [cs] (lexeme (k/none-of* cs)))

        token
        (if (:case-sensitive rec)
          (fn ([cs] (lexeme (k/token* cs)))
            ([cs & more] (lexeme (apply k/token* cs more))))
          (fn ([cs] (lexeme (k/token- cs)))
            ([cs & more] (lexeme (apply k/token- cs more)))))

        word
        (let [il (:identifier-letter rec)]
          (if (:case-sensitive rec)
            (fn ([cs] (lexeme (k/word* il cs)))
              ([cs & more] (lexeme (apply k/word* il cs more))))
            (fn ([cs] (lexeme (k/word- il cs)))
              ([cs & more] (lexeme (apply k/word- il cs more))))))

        identifier
        (let [start (:identifier-start rec)
              other (:identifier-letter rec)
              names (:reserved-names rec)
              elem? (fn [s coll] (k/member? (lower-case s) (map #(lower-case %) coll)))
              is-in (if (:case-sensitive rec) k/member? elem?)
              check (fn [p] (>>= p (fn [s] (if (is-in s names)
                                             (k/fail (fmt :reserved s))
                                             (k/return s)))))
              t     (:type rec)]
          (cond (= t :basic) (<:> (check (lexeme (<+> start (k/many0 other)))))
                (= t :C) (<:> (check (lexeme (<+> start (k/many0 other)))))
                (= t :Haskell) (<:> (check (lexeme (<+> start (k/many0 other)))))
                (= t :Java) (<:> (check (lexeme (<+> start (k/many0 other)))))
                (= t :Shell) (<:> (check (lexeme (<+> start (k/many0 other)))))))

        field
        (fn [cs] (lexeme (k/field* cs)))

        char-lit
        (let [t (:type rec)]
          (cond (= t :basic) (char-parser lexeme basic-char)
                (= t :C) (char-parser lexeme c-char)
                (= t :Haskell) (char-parser lexeme haskell-char)
                (= t :Java) (char-parser lexeme java-char)
                (= t :Shell) (char-parser lexeme c-char)))

        string-lit
        (let [t (:type rec)]
          (cond (= t :basic) (str-parser lexeme basic-char)
                (= t :C) (<+> (k/many1 (str-parser lexeme c-char)))
                (= t :Haskell) (str-parser lexeme haskell-char)
                (= t :Java) (str-parser lexeme java-char)
                (= t :Shell) (<+> (k/many1 (str-parser lexeme c-char)))))

        dec-lit
        (let [lead (if (:leading-sign rec) sign (k/return nil))]
          (<?> (>>= (<:> (lexeme (<+> lead (k/many1 k/digit) int-suffix)))
                    (fn [x] (k/return (read-num-lit x))))
               (i18n :dec-lit)))

        oct-lit
        (let [lead (if (:leading-sign rec) sign (k/return nil))]
          (<?> (>>= (<:> (lexeme (<+> lead (k/sym* \0) (k/many k/oct-digit) int-suffix)))
                    (fn [x] (k/return (read-num-lit x))))
               (i18n :oct-lit)))

        hex-lit
        (let [lead (if (:leading-sign rec) sign (k/return nil))]
          (<?> (>>= (<:> (lexeme (<+> lead (k/token- "0x") (k/many1 k/hex-digit) int-suffix)))
                    (fn [x] (k/return (read-num-lit x))))
               (i18n :hex-lit)))

        float-lit
        (let [lead (if (:leading-sign rec) sign (k/return nil))]
          (<?> (>>= (<:> (lexeme
                           (<+> lead (k/many1 k/digit)
                                (k/option ".0" (<*> (k/sym* \.) (k/many1 k/digit)))
                                (k/optional (<*> (k/one-of* "eE") sign (k/many1 k/digit)))
                                float-suffix)))
                    (fn [x] (>> (k/return (read-num-lit x)) k/clear-empty)))
               (i18n :float-lit)))

        bool-lit
        (<|> (>> (word "true") (k/return true))
             (>> (word "false") (k/return false)))

        nil-lit
        (>> (word "nil" "null") (k/return nil))

        parens
        (fn [p] (k/between (sym \() (sym \)) (lexeme p)))

        braces
        (fn [p] (k/between (sym \{) (sym \}) (lexeme p)))

        angles
        (fn [p] (k/between (sym \<) (sym \>) (lexeme p)))

        brackets
        (fn [p] (k/between (sym \[) (sym \]) (lexeme p)))

        semi
        (sym \;)

        comma
        (sym \,)

        colon
        (sym \:)

        dot
        (sym \.)

        semi-sep
        (fn [p] (k/sep-by semi (lexeme p)))

        semi-sep1
        (fn [p] (k/sep-by1 semi (lexeme p)))

        comma-sep
        (fn [p] (k/sep-by comma (lexeme p)))

        comma-sep1
        (fn [p] (k/sep-by1 comma (lexeme p)))]

    (map->TokenParsers
      {:trim       trim
       :lexeme     lexeme
       :sym        sym
       :new-line   new-line
       :one-of     one-of
       :none-of    none-of
       :token      token
       :word       word
       :identifier identifier
       :field      field
       :char-lit   char-lit
       :string-lit string-lit
       :dec-lit    dec-lit
       :oct-lit    oct-lit
       :hex-lit    hex-lit
       :float-lit  float-lit
       :bool-lit   bool-lit
       :nil-lit    nil-lit
       :parens     parens
       :braces     braces
       :angles     angles
       :brackets   brackets
       :semi       semi
       :comma      comma
       :colon      colon
       :dot        dot
       :semi-sep   semi-sep
       :semi-sep1  semi-sep1
       :comma-sep  comma-sep
       :comma-sep1 comma-sep1})))
