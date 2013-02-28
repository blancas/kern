;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Kern Lexer library."
      :author "Armando Blancas"}
  blancas.kern.lexer
  (:use [blancas.kern core i18n]
	[clojure.string :only (lower-case)]))

		
;; +-------------------------------------------------------------+
;; |                    Language definitions.                    |
;; +-------------------------------------------------------------+


;; A language def record customizes the parsers returned by make-parsers.
(defrecord LanguageDef
  [type                ;; Identifies the type of settings.
   comment-start       ;; A string that marks the start of a block comment.
   comment-end         ;; A string that marks the end of a block comment.
   comment-line        ;; A string that marks the start of a line comment.
   nested-comments     ;; Whether the lexer accepts nested comments; a boolean.
   identifier-start    ;; A parser for the start of an identifier.
   identifier-letter   ;; A parser for the subsequent characters of an identifier.
   reserved-names      ;; A list of names that cannot be identifiers.
   case-sensitive      ;; Whether tokens are case-sensitive; a boolean.
   line-continuation   ;; A parser for the token that precedes the new line.
   trim-newline])      ;; Treats newline character(s) as whitespace.


(def basic-def
  "The most basic record; for use to build new styles."
  (map->LanguageDef
    {:type               :basic
     :comment-start      ""
     :comment-end        ""
     :comment-line       ""
     :nested-comments    false
     :identifier-start   (<|> letter (sym* \_))
     :identifier-letter  (<|> alpha-num (sym* \_))
     :reserved-names     []
     :case-sensitive     true
     :line-continuation  (sym* \\)
     :trim-newline       true}))


(def haskell-style
  "Lexical settings for Haskell-style languages."
  (assoc basic-def
    :type                :Haskell
    :comment-start       "{-"
    :comment-end         "-}"
    :comment-line        "--"
    :nested-comments     true))


(def java-style
  "Lexical settings for Java-style languages."
  (assoc basic-def
    :type                :Java
    :comment-start       "/*"
    :comment-end         "*/"
    :comment-line        "//"))


(def c-style
  "Lexical settings for C-style languages."
  (assoc java-style
    :type                :C))


(def shell-style
  "Lexical settings for shell-style languages."
  (assoc basic-def
    :type                :Shell
    :comment-line        "#"
    :identifier-letter   (<|> alpha-num (one-of* "_-."))
    :trim-newline        false))


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


(defn* trim
  "Skips over any whitespace, including comments (if defined), at
   the start of the input. Whether newline characters are removed
   as whitespace is configured by :trim-newline. When that setting
   is true, the setting :line-continuation is activated." 
  [] nil)

(defn* lexeme
  "Applies parser p, then calls (trim)."
  [p] nil)

(defn* sym
  "Parses a single character c. Compares according to
   :case-sensitive. Calls (trim) afterwards."
  [c] nil)

(defn* new-line
  "Parses a new line, UNIX or Windows style; then calls (trim)."
  [] nil)

(defn* one-of
  "Succeeds if the next character is in the supplied string.
   Calls (trim) afterwards."
  [cs] nil)

(defn* none-of
  "Succeeds if the next character is not in the supplied string.
   Calls (trim) afterwards."
  [] nil)

(defn* token
  "Parses a specific string, not necessarily delimited. If more
   than one are given it will try each choice in turn. Compares
   according to :case-sensitive. Calls (trim) afterwards."
  ([cs] nil)
  ([cs & more] nil))

(defn* word
  "Parses a specific string; must be delimited by any character not
   parsed by :identifier-letter. If more than one are given it will
   try each choice in turn. Compares according to :case-sensitive.
   Calls (trim) afterwards."
  ([cs] nil)
  ([cs & more] nil))

(defn* identifier
  "Parses an unquoted string suitable for an identifier or a name.
   The start of the input is defined by :identifier-start, and
   subsequent symbols by :identtifier-letter. It will check that
   the parsed value not be in the list of :reserved-names, if any,
   comparing according to :case-sensitive. Calls (trim) afterwards."
  [] nil)

(defn* field
  "Parses an unquoted text field terminated by any character
   in cs. Calls (trim) afterwards."
  [cs] nil)

(defn* char-lit
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

(defn* string-lit
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

(defn* dec-lit
  "Parses a decimal number as Long or BigInt depending on the
   magnitude or if it ends with N. Calls (trim) afterward."
  [] nil)

(defn* oct-lit
  "Parses an octal number as Long or BigInt depending on the
   magnitude or if it ends with N. Calls (trim) afterward."
  [] nil)

(defn* hex-lit
  "Parses a hexadecimal number as Long or BigInt depending on the
   magnitude or if it ends with N. Calls (trim) afterward."
  [] nil)

(defn* float-lit
  "Parses a floating-point number as Double or BigDecimal depending
   on the magnitude or if it ends with M. It cannot start with a
   period. The first period found must be followed by at least one
   digit. Calls (trim) afterward."
  [] nil)

(defn* bool-lit
  "Parses a boolean value, true or false, comparing according to
   :case-sensitive. Calls (trim) afterward."
  [] nil)

(defn* nil-lit
  "Parses a null value, nil or null, comparing according to
   :case-sensitive. Calls (trim) afterward."
  [] nil)

(defn* parens
  "Applies parser p skiping over surrounding parenthesis.
   Calls (trim) after the opening paren, after p, and after
   the closing paren."
  [p] nil)

(defn* braces
  "Applies parser p skiping over surrounding braces.
   Calls (trim) after the opening brace, after p, and after
   the closing brace."
  [p] nil)

(defn* angles
  "Applies parser p skiping over surrounding angle brackets.
   Calls (trim) after the opening bracket, after p, and after
   the closing bracket."
  [p] nil)

(defn* brackets
  "Applies parser p skiping over surrounding brackets.
   Calls (trim) after the opening bracket, after p, and after
   the closing bracket."
  [p] nil)

(defn* semi
  "Parses a single semicolon; then calls (trim)."
  [] nil)

(defn* comma
  "Parses a single comma; then calls (trim)."
  [] nil)

(defn* colon
  "Parses a single colon; then calls (trim)."
  [] nil)

(defn* dot
  "Parses a single dot; then calls (trim)."
  [] nil)

(defn* semi-sep
  "Applies parser p zero or more times, skiping over separating
   semicolons. Calls (trim) after each p and semicolon."
  [] nil)

(defn* semi-sep1
  "Applies parser p one or more times, skiping over separating
   semicolons. Calls (trim) after each p and semicolon."
  [] nil)

(defn* comma-sep
  "Applies parser p zero or more times, skiping over separating
   commas. Calls (trim) after each p and comma."
  [] nil)

(defn* comma-sep1
  "Applies parser p one or more times, skiping over separating
   commas. Calls (trim) after each p and comma."
  [] nil)


(defmacro with-parsers
  "Binds the parser vars in the kern.lexer namespace to the values in rec."
  [rec & body]
  (list 'binding
	['blancas.kern.lexer/trim        (list :trim       rec)
	 'blancas.kern.lexer/lexeme      (list :lexeme     rec)
	 'blancas.kern.lexer/sym         (list :sym        rec)
	 'blancas.kern.lexer/new-line    (list :new-line   rec)
	 'blancas.kern.lexer/one-of      (list :one-of     rec)
	 'blancas.kern.lexer/none-of     (list :none-of    rec)
	 'blancas.kern.lexer/token       (list :token      rec)
	 'blancas.kern.lexer/word        (list :word       rec)
	 'blancas.kern.lexer/identifier  (list :identifier rec)
	 'blancas.kern.lexer/field       (list :field      rec)
	 'blancas.kern.lexer/char-lit    (list :char-lit   rec)
	 'blancas.kern.lexer/string-lit  (list :string-lit rec)
	 'blancas.kern.lexer/dec-lit     (list :dec-lit    rec)
	 'blancas.kern.lexer/oct-lit     (list :oct-lit    rec)
	 'blancas.kern.lexer/hex-lit     (list :hex-lit    rec)
	 'blancas.kern.lexer/float-lit   (list :float-lit  rec)
	 'blancas.kern.lexer/bool-lit    (list :bool-lit   rec)
	 'blancas.kern.lexer/nil-lit     (list :nil-lit    rec)
	 'blancas.kern.lexer/parens      (list :parens     rec)
	 'blancas.kern.lexer/braces      (list :braces     rec)
	 'blancas.kern.lexer/angles      (list :angles     rec)
	 'blancas.kern.lexer/brackets    (list :brackets   rec)
	 'blancas.kern.lexer/semi        (list :semi       rec)
	 'blancas.kern.lexer/comma       (list :comma      rec)
	 'blancas.kern.lexer/colon       (list :colon      rec)
	 'blancas.kern.lexer/dot         (list :dot        rec)
	 'blancas.kern.lexer/semi-sep    (list :semi-sep   rec)
	 'blancas.kern.lexer/semi-sep1   (list :semi-sep1  rec)
	 'blancas.kern.lexer/comma-sep   (list :comma-sep  rec)
	 'blancas.kern.lexer/comma-sep1	 (list :comma-sep1 rec)]
	(cons 'do body)))


;; +-------------------------------------------------------------+
;; |              Private functions for whitespace.              |
;; +-------------------------------------------------------------+


(defn- line-comment
  "Parses a line comment."
  [rec]
  (let [start (:comment-line rec)]
    (>>= (token* start)
         (fn [_] (>>= (many-till any-char (<|> new-line* eof))
                      (fn [_] (return nil)))))))


(defn- block-nested
  "Parses the contents and end of a nested block comment."
  [rec]
  (let [start (:comment-start rec)
	end (:comment-end rec)]
    (expect (many-till (<|> (>>= (<:> (token* start)) (fn [_] (block-nested rec)))
			    any-char)
		       (token* end))
	    (i18n :end-comment))))


(defn- block-rest
  "Parses the contents and end of a block comment."
  [rec]
  (let [end (:comment-end rec)]
    (expect (many-till any-char (token* end)) (i18n :end-comment))))


(defn- block-comment
  "Parses a block comment."
  [rec]
  (let [start (:comment-start rec)
        nest? (:nested-comments rec)]
    (skip (<:> (token* start))
               (if nest? (block-nested rec) (block-rest rec)))))


;; +-------------------------------------------------------------+
;; |    Private functions for character and string literals.     |
;; +-------------------------------------------------------------+


(def- esc-char
  "Parses an escape code for a basic char."
  (let [codes (zipmap "btnfr'\"\\/" "\b\t\n\f\r'\"\\/")]
    (>>= (<?> (one-of* "btnfr'\"\\/") (i18n :esc-code))
	 (fn [x] (return (get codes x))))))


(defn- basic-char
  "Parses an unquoted character literal. Character c must be escaped."
  [c]
  (<?> (<|> (satisfy #(and (not= % c) (not= % \\) (>= (int %) (int \space))))
	    (>> (sym* \\) esc-char))
       (i18n :char-lit)))


(def- esc-oct
  "Parses an octal escape code; the result is the encoded char."
  (>>= (<+> (many1 oct-digit))
       (fn [x]
	 (let [n (Integer/parseInt x 8)]
	   (if (<= n 0377)
	     (return (char n))
	     (fail (i18n :bad-octal)))))))


(def- esc-uni
  "Parses a unicode escape code; the result is the encoded char."
  (>>= (<+> (>> (sym* \u) (times 4 hex-digit)))
       (fn [x] (return (aget (Character/toChars (Integer/parseInt x 16)) 0)))))


(defn- java-char
  "Parses an unquoted Java character literal. Character c must be escaped."
  [c]
  (<?> (<|> (satisfy #(and (not= % c) (not= % \\) (>= (int %) (int \space))))
	    (>> (sym* \\)
		(<?> (<|> esc-char esc-oct esc-uni)
		     (i18n :esc-code-j))))
       (i18n :char-lit)))


(def- c-esc-char
  "Parses an escape code for a C char."
  (let [codes (assoc (zipmap "btnfr'\"\\?/" "\b\t\n\f\r'\"\\?/")
		     \a (char 7) \v (char 11) \0 (char 0))]
    (>>= (one-of* "btnfr'\"\\?/av0") (fn [x] (return (get codes x))))))


(def- c-esc-uni
  "Parses a long unicode escape code; the result is the encoded char."
  (>>= (<+> (>> (sym* \U) (times 8 hex-digit)))
       (fn [x] (return (aget (Character/toChars (Integer/parseInt x 16)) 0)))))


(def- c-esc-hex
  "Parses a hex escape code; the result is the encoded char."
  (>>= (<+> (>> (sym- \x) (times 2 hex-digit)))
       (fn [x] (return (aget (Character/toChars (Integer/parseInt x 16)) 0)))))


(defn- c-char
  "Parses an unquoted C character literal. Character c must be escaped."
  [c]
  (<?> (<|> (satisfy #(and (not= % c) (not= % \\) (>= (int %) (int \space))))
	    (>> (sym* \\)
		(<?> (<|> c-esc-hex c-esc-char esc-oct esc-uni c-esc-uni)
		     (i18n :esc-code-c))))
       (i18n :char-lit)))


(def- h-esc-oct
  "Parses a Haskell octal escape code; the result is the encoded char."
  (>>= (<+> (>> (sym* \o) (many1 oct-digit)))
       (fn [x]
	 (let [n (Integer/parseInt x 8)]
	   (if (<= n 04177777)
	     (return (char n))
	     (fail (i18n :bad-oct-h)))))))


(def- h-esc-dec
  "Parses a Haskell decimal escape code; the result is the encoded char."
  (>>= (<+> (many1 digit))
       (fn [x]
	 (let [n (Integer/parseInt x)]
	   (if (<= n 1114111)
	     (return (char n))
	     (fail (i18n :bad-dec-h)))))))


(def- h-esc-hex
  "Parses a Haskell hex escape code; the result is the encoded char."
  (>>= (<+> (>> (sym* \x) (many1 hex-digit)))
       (fn [x]
	 (let [n (Integer/parseInt x 16)]
	   (if (<= n 0x10ffff)
	     (return (char n))
	     (fail (i18n :bad-hex-h)))))))


(defn- haskell-char
  "Parses Haskell character literals."
  [c]
  (<?> (<|> (satisfy #(and (not= % c) (not= % \\) (>= (int %) (int \space))))
	    (>> (sym* \\)
		(<?> (<|> h-esc-hex h-esc-oct c-esc-char h-esc-dec)
		     (i18n :esc-code-h))))
       (i18n :char-lit)))


(defn- char-parser
  "Parses character literals delimited by single quotes."
  [lex f]
  (<?> (lex (between (sym* \') (<?> (sym* \') (i18n :end-char)) (f \')))
       (i18n :char-lit)))


(defn- str-parser
  "Parses string literals delimited by double quotes."
  [lex f]
  (<?> (lex (between (sym* \")
		     (<?> (sym* \") (i18n :end-string))
		     (<+> (many (f \")))))
       (i18n :string-lit)))


;; +-------------------------------------------------------------+
;; |          Private functions for numeric literals.            |
;; +-------------------------------------------------------------+


(def- sign (optional (one-of* "+-")))

(def- int-suffix (<|> (<< (sym* \N) (not-followed-by letter))
		      (not-followed-by (<|> letter (sym* \.)))))

(def- float-suffix (<< (optional (sym* \M)) (not-followed-by letter)))


;; +-------------------------------------------------------------+
;; |                      Parser generator.                      |
;; +-------------------------------------------------------------+


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
   :trim-newline])      Treats newline character(s) as whitespace."
  [rec]
  (let [trim
	(let [line?   (seq (:comment-line rec))
	      multi?  (seq (:comment-start rec))
	      both?   (and line? multi?)
	      ws      (if (:trim-newline rec)
			white-space
			(<|> (one-of* "\t\f\r ")
			     (skip (:line-continuation rec) new-line*)))
	      many-ws (skip-many1 ws)]
          (cond both?  (skip-many (<|> many-ws (line-comment rec) (block-comment rec)))
	        line?  (skip-many (<|> many-ws (line-comment rec)))
	        multi? (skip-many (<|> many-ws (block-comment rec)))
	        :else  (skip-many ws)))

	lexeme
        (fn [p] (<< p trim))

	sym
	(if (:case-sensitive rec)
          (fn [x] (lexeme (sym* x)))
          (fn [x] (lexeme (sym- x))))

	new-line
	(lexeme new-line*)
	
        one-of
        (fn [cs] (lexeme (one-of* cs)))
	
        none-of
	(fn [cs] (lexeme (none-of* cs)))

	token
	(if (:case-sensitive rec)
          (fn ([cs] (lexeme (token* cs)))
	      ([cs & more] (lexeme (apply token* cs more))))
          (fn ([cs] (lexeme (token- cs)))
	      ([cs & more] (lexeme (apply token- cs more)))))

	word
	(let [il (:identifier-letter rec)]
	  (if (:case-sensitive rec)
            (fn ([cs] (lexeme (word* il cs)))
	        ([cs & more] (lexeme (apply word* il cs more))))
            (fn ([cs] (lexeme (word- il cs)))
	        ([cs & more] (lexeme (apply word- il cs more))))))

	identifier
	(let [start (:identifier-start rec)
	      other (:identifier-letter rec)
	      names (:reserved-names rec)
	      elem? (fn [s coll] (member? (lower-case s) (map #(lower-case %) coll)))
	      is-in (if (:case-sensitive rec) member? elem?)
	      check (fn [p] (>>= p (fn [s] (if (is-in s names)
					     (fail (fmt :reserved s))
					     (return s)))))
	      t     (:type rec)]
	  (cond (= t :basic)   (<:> (check (lexeme (<+> start (many other)))))
		(= t :C)       (<:> (check (lexeme (<+> start (many other)))))
		(= t :Haskell) (<:> (check (lexeme (<+> start (many other)))))
		(= t :Java)    (<:> (check (lexeme (<+> start (many other)))))
		(= t :Shell)   (<:> (check (lexeme (<+> start (many other)))))))

	field
        (fn [cs] (lexeme (field* cs)))

	char-lit
	(let [t (:type rec)]
	  (cond (= t :basic)   (char-parser lexeme basic-char)
		(= t :C)       (char-parser lexeme c-char)
		(= t :Haskell) (char-parser lexeme haskell-char)
		(= t :Java)    (char-parser lexeme java-char)
		(= t :Shell)   (char-parser lexeme c-char)))

	string-lit
	(let [t (:type rec)]
	  (cond (= t :basic)   (str-parser lexeme basic-char)
		(= t :C)       (<+> (many1 (str-parser lexeme c-char)))
		(= t :Haskell) (str-parser lexeme haskell-char)
		(= t :Java)    (str-parser lexeme java-char)
		(= t :Shell)   (<+> (many1 (str-parser lexeme c-char)))))

	dec-lit
	(<?> (>>= (<:> (lexeme (<+> sign (many1 digit) int-suffix)))
                  (fn [x] (return (read-string x))))
             (i18n :dec-lit))
	
	oct-lit
	(<?> (>>= (<:> (lexeme (<+> sign (sym* \0) (many oct-digit) int-suffix)))
                  (fn [x] (return (read-string x))))
             (i18n :oct-lit))

	hex-lit
	(<?> (>>= (<:> (lexeme (<+> sign (token- "0x") (many1 hex-digit) int-suffix)))
                  (fn [x] (return (read-string x))))
             (i18n :hex-lit))

	float-lit
	(<?> (>>= (<:> (lexeme
		         (<+> sign (many1 digit)
	                      (option ".0" (<*> (sym* \.) (many1 digit)))
	                      (optional (<*> (one-of* "eE") sign (many1 digit)))
			      float-suffix)))
                  (fn [x] (return (read-string x))))
             (i18n :float-lit))

	bool-lit
        (<|> (>> (word "true") (return true))
             (>> (word "false") (return false)))

	nil-lit
	(>> (word "nil" "null") (return nil))
	
	parens
	(fn [p] (between (sym \() (sym \)) (lexeme p)))

	braces
	(fn [p] (between (sym \{) (sym \}) (lexeme p)))

	angles
	(fn [p] (between (sym \<) (sym \>) (lexeme p)))
	
	brackets
	(fn [p] (between (sym \[) (sym \]) (lexeme p)))
	
	semi
	(sym \;)
	
	comma
	(sym \,)
	
	colon
	(sym \:)
	
	dot
	(sym \.)
	
	semi-sep
	(fn [p] (sep-by semi (lexeme p)))
	
	semi-sep1
	(fn [p] (sep-by1 semi (lexeme p)))
	
	comma-sep
	(fn [p] (sep-by comma (lexeme p)))

	comma-sep1
	(fn [p] (sep-by1 comma (lexeme p)))]

    (map->TokenParsers
      {:trim        trim
       :lexeme      lexeme
       :sym         sym
       :new-line    new-line
       :one-of      one-of
       :none-of     none-of
       :token       token
       :word        word
       :identifier  identifier
       :field       field
       :char-lit    char-lit
       :string-lit  string-lit
       :dec-lit     dec-lit
       :oct-lit     oct-lit
       :hex-lit     hex-lit
       :float-lit   float-lit
       :bool-lit    bool-lit
       :nil-lit     nil-lit
       :parens      parens
       :braces      braces
       :angles      angles
       :brackets    brackets
       :semi        semi
       :comma       comma
       :colon       colon
       :dot         dot
       :semi-sep    semi-sep
       :semi-sep1   semi-sep1
       :comma-sep   comma-sep
       :comma-sep1  comma-sep1})))
