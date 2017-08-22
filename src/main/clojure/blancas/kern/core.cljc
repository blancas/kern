;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns
  ^{:doc    "The core Kern library.

Kern is a library of parser combinators for Clojure. It is useful for
implementing recursive-descent parsers based on predictive LL(1) grammars
with on-demand, unlimited look-ahead LL(*).

The main inspiration for Kern comes from Parsec, a Haskell library written
by Daan Leijen, as well as work by Graham Hutton, Erik Meijer, and William Burge.
The name Kern is a token of appreciation to Brian Kernighan (now at Princeton)
for his work on programming languages.

Daan Leijen
Parsec, a fast combinator parser, 2001
http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf

Graham Hutton and Erik Meijer
Monadic Parser Combinators, 1996
http://eprints.nottingham.ac.uk/237/1/monparsing.pdf

William H. Burge
Recursive Programming Techniques
Addison-Wesley, 1975"
    :author "Armando Blancas"}
  blancas.kern.core
  (:refer-clojure :exclude [cat])
  (:require [blancas.kern.i18n :refer [fmt i18n di18n]]
            [blancas.kern.char :as char]
            [clojure.string :refer [join] :as str]
            [clojure.pprint :refer [pprint]]))


(defmacro defn*
  "Same as def, yielding a dynamic def."
  [name & more]
  (list* `defn (with-meta name (assoc (meta name) :dynamic true)) more))


#?(:clj
   (defmacro fwd
     "Delays the evaluation of a parser that was forward (declare)d and
it has not been defined yet. For use in (def)s of no-arg parsers,
since the parser expression evaluates immediately."
     [p]
     (let [x (gensym)]
       `(fn [~x] (~p ~x)))))


#?(:clj
   (defn char-seq
     "Returns characters from rdr as a lazy sequence.
      rdr must implement java.io.Reader"
     [^java.io.Reader rdr]
     (let [c (.read rdr)]
       (when-not (neg? c)
         (cons (char c) (lazy-seq (char-seq rdr)))))))


#?(:clj
   (defn f->s
     "Gets a character sequence from a file-like object."
     ([f] (slurp f))
     ([f e] (slurp f :encoding e))))


(defn member?
  "Tests if x is a member of coll."
  [x coll] (some #{x} coll))


(defn- conjp
  "Like conj but ignores values based on a predicate."
  [p] (fn [coll x] (if (p x) coll (conj coll x))))


(def ^:dynamic *tab-width*
  "The number of columns to advance for a tab character.
   By default, a tab takes four columns."
  4)


;; +-------------------------------------------------------------+
;; |                      Data structures.                       |
;; +-------------------------------------------------------------+


;; Error types.
(def err-system 0)                                          ;; Used in satisfy for specific unexpected input.
(def err-unexpect 1)                                        ;; Used on any unexpected input to show a message.
(def err-expect 2)                                          ;; Used to show a message of what's expected.
(def err-message 3)                                         ;; Used for any kind of message from client code.

;; Keeps the position of the input:
;; src   - a string that identifies the source of input
;; line  - the line into the input stream.
;; col   - the column into the line.
(defrecord PPosition [src line col]
  #?(:clj Comparable :cljs IComparable)
  (#?(:clj compareTo :cljs -compare) [this other]
    (let [result (compare line (:line other))]
      (if (zero? result)
        (compare col (:col other))
        result))))

;; A PMessage consists of:
;; type  - One of the error types listed above.
;; text  - The text of the message.
(defrecord PMessage [type text])

;; A PError consists of:
;; pos   - A position into the input stream.
;; msgs  - A sequence of messages.
(defrecord PError [pos msgs])

;; The state of a parser consists of:
;; input - The input sequence.
;; pos   - The position into the input.
;; value - The value of the parsed input.
;; ok    - Whether the parser terminated without error.
;; empty - Whether the parser consumed nothing from the input.
;; user  - An object stored by the client code.
;; error - Any errors collected during parsing.
(defrecord PState [input pos value ok empty user error])


(defn make-pos
  "Makes a position record."
  ([src] (make-pos src 1 1))
  ([src ln col] (->PPosition (or src "") ln col)))


(defn ^:dynamic char-pos
  "Computes the new position of the character c."
  [pos c]
  (cond (= c \newline) (assoc pos :col 1 :line (inc (:line pos)))
        (= c \tab) (assoc pos :col (+ (:col pos) *tab-width*))
        :else (assoc pos :col (inc (:col pos)))))


(defn ^:dynamic str-pos
  "Computes the stream position after the character sequence cs."
  [pos cs] (if (empty? cs) pos (recur (char-pos pos (first cs)) (rest cs))))


(defn make-err-system
  "Makes a message of type err-system."
  [pos text] (->PError pos (list (->PMessage err-system text))))


(defn make-err-unexpect
  "Makes a message of type err-unexpect."
  [pos text] (->PError pos (list (->PMessage err-unexpect text))))


(defn make-err-expect
  "Makes a message of type err-expect."
  [pos text] (->PError pos (list (->PMessage err-expect text))))


(defn make-err-message
  "Makes a message of type err-message."
  [pos text] (->PError pos (list (->PMessage err-message text))))


(defn get-msg
  "Get the text from message types system, unexpect, and message."
  [pmsg]
  (let [type (:type pmsg)
        text (-> pmsg :text force)]
    (cond (= type err-system) (fmt :unexpected text)
          (= type err-unexpect) (fmt :unexpected text)
          (= type err-message) text)))


(defn get-msg-expect
  "Get the text from a list of messages of type expect."
  [lst]
  (let [show (fn [xs]
               (let [comma-sep (join (i18n :comma) (butlast xs))
                     or-last   (fmt :or (last xs))]
                 (str comma-sep or-last)))
        opts (map (comp force :text) lst)
        cnt  (count opts)]
    (fmt :expecting (if (= cnt 1) (first opts) (show opts)))))


(defn- get-msg-list
  "Gets the text of error messages as a list."
  [{msgs :msgs}]
  (let [ms (distinct msgs)]
    (concat
      (let [lst (filter #(= (:type %) err-system) ms)]
        (reduce #(conj %1 (get-msg %2)) [] lst))
      (let [lst (filter #(= (:type %) err-unexpect) ms)]
        (reduce #(conj %1 (get-msg %2)) [] lst))
      (let [lst (filter #(= (:type %) err-expect) ms)]
        (if (empty? lst) lst (list (get-msg-expect lst))))
      (let [lst (filter #(= (:type %) err-message) ms)]
        (reduce #(conj %1 (get-msg %2)) [] lst)))))


(defn get-msg-str
  "Gets the text of error messages separated by \\n."
  [err]
  (let [eol #?(:clj (System/getProperty "line.separator") :cljs "\n")]
    (join eol (get-msg-list err))))


(defn- ^:dynamic merge-err
  "Merges errors from two state records."
  [{e1 :error} {e2 :error}]
  (cond (and (nil? e1) (nil? e2)) nil
        (nil? e1) e2
        (nil? e2) e1
        :else (let [pos1 (:pos e1) pos2 (:pos e2)
                    r    (compare [(:line pos1) (:col pos1)] [(:line pos2) (:col pos2)])]
                (cond (zero? r) (update-in e1 [:msgs] concat (:msgs e2))
                      (pos? r) e1
                      :else e2))))


(defn- cat
  "Applies p on s; conjoins the result to that of s."
  [p s]
  (let [st (p s)]
    (if (:ok st)
      (assoc st :value (conj (:value s) (:value st)))
      (assoc st :empty (and (:empty s) (:empty st))))))


(defn- ^:dynamic set-ex
  "Replace expect errors with expecting msg."
  [msg s]
  (letfn [(not-ex [{type :type}]
            (not (= type err-expect)))
          (update [lst err]
            (cons err (filter not-ex lst)))]
    (let [m (->PMessage err-expect msg)]
      (update-in s [:error :msgs] update m))))


;; +-------------------------------------------------------------+
;; |              Public supporting functions.                   |
;; +-------------------------------------------------------------+


(defn reply
  "Makes s succeed with value v."
  [v s] (assoc s :value v :ok true :error nil))


(defn failed-empty?
  "Tests if s failed without consuming any input."
  [s] (and (not (:ok s)) (:empty s)))


(defn unexpected
  "Sets s as failed because an unexpected reason."
  [msg s]
  (let [err (make-err-unexpect (:pos s) msg)]
    (assoc s :value nil :ok false :empty true :error err)))


(defn unexpected-input
  "Sets s as failed because of an unexpected input."
  [in s]
  (let [err (make-err-system (:pos s) in)]
    (assoc s :value nil :ok false :empty true :error err)))


(defn expecting
  "Sets s as expecting msg."
  [msg s] (assoc s :error (make-err-expect (:pos s) msg)))


(defn clear-empty
  "Sets the parser state as not empty. Needed in compound parsers
   where optional parsers at the end may leave an incorrect :empty
   state for the parser as a whole."
  [s] (assoc s :empty false))


;; +-------------------------------------------------------------+
;; |                       Basic parsers.                        |
;; +-------------------------------------------------------------+


(defn return
  "Succeeds without consuming any input. Any carried errors
   are removed."
  [v] (fn [s] (assoc s :value v :ok true :error nil)))


(defn fail
  "Fails without consuming any input, having a single error
   record with the passed messge msg."
  [msg]
  (fn [s]
    (let [err (make-err-message (:pos s) msg)]
      (assoc s :value nil :ok false :empty true :error err))))


(defn satisfy
  "Succeeds if the next character satisfies the predicate pred,
   in which case advances the position of the input stream. It
   may fail on an unexpected end of input."
  [pred]
  (fn [s]
    (let [stm (:input s)]
      (if (empty? stm)
        (unexpected (i18n :eof) s)
        (let [c (first stm)]
          (if (pred c)
            (->PState (rest stm) (char-pos (:pos s) c) c true false (:user s) nil)
            (unexpected-input (with-out-str (pr c)) s)))))))


;; +-------------------------------------------------------------+
;; |                     Parser combinators.                     |
;; +-------------------------------------------------------------+


(defn <?>
  "If parser p fails consuming no input, it replaces any Expecting
   errors with a single Expecting with message msg. This helps to
   produce more abstract and accurate error messages."
  [p msg]
  (fn [s]
    (let [st (p s)]
      (if (failed-empty? st) (set-ex msg st) st))))


(defn expect
  "Applies parser p; if it fails (regardless of input consumed)
   it replaces any expecting errors with expecting msg. This is
   similar to <?> but works even if some input was consumed."
  [p msg]
  (fn [s]
    (let [st (p s)]
      (if (:ok st) st (set-ex msg st)))))


(defn <|>
  "Tries p; if it fails without consuming any input, it tries q.
   With more parsers, it will stop and succeed if a parser succeeds;
   it will stop and fail if a parser fails consuming input; or it
   will try the next one if a parser fails without consuming input."
  ([p q]
   (fn [s]
     (let [s2 (p s)]
       (if (failed-empty? s2)
         (let [s3 (q s)]
           (if (:ok s3)
             s3
             (assoc s3 :error (merge-err s2 s3))))
         s2))))
  ([p q & more]
   (reduce <|> (list* p q more))))


(defn >>=
  "Binds parser p to function f which gets p's value and returns
   a new parser. Function p must define a single parameter. The
   argument it receives is the value parsed by p, not ps' return
   value, which is a parser state record."
  [p f]
  (fn [s]
    (let [s1 (p s)]
      (if (:ok s1)
        (let [s2 ((f (:value s1)) s1)
              s3 (assoc s2 :empty (and (:empty s1) (:empty s2)))]
          (if (:ok s3)
            s3
            (assoc s3 :error (merge-err s1 s3))))
        s1))))

#?(:clj
   (defmacro bind
     "Expands into nested >>= forms and a function body. The pattern:

   (>>= p1 (fn [v1]
   (>>= p2 (fn [v2]
   ...
     (return (f v1 v2 ...))))))

   can be more conveniently be written as:

   (bind [v1 p1 v2 p2 ...] (return (f v1 v2 ...)))"
     [[& bindings] & body]
     (let [[sym p] (take 2 bindings)]
       (if (= 2 (count bindings))
         `(>>= ~p (fn [~sym] ~@body))
         `(>>= ~p (fn [~sym] (bind ~(drop 2 bindings) ~@body)))))))


(defn >>
  "Parses p followed by q; skips p, keeps q. If more parsers are
   given, it skips all but last and keeps the result of the last."
  ([p q]
   (>>= p (fn [_] q)))
  ([p q & more]
   (reduce >> (list* p q more))))


(defn <<
  "Parses p followed by q; keeps p, skips q. If more parsers are
   given, it keeps the first result and skips the rest."
  ([p q]
   (>>= p (fn [x] (>> q (return x)))))
  ([p q & more]
   (reduce << (list* p q more))))


(defn <$>
  "Parses p; if successful, it applies f to the value parsed by p."
  [f p] (>>= p (fn [x] (return (f x)))))


(defn <*>
  "Applies one or more parsers; collects the results in a
   vector, including nil values. If any parser fails, it
   stops immediately and fails."
  [p & more]
  (fn [s]
    (loop [s1 (assoc s :value [] :empty true) ps (cons p more)]
      (let [s2 (cat (first ps) s1)]
        (if (and (:ok s2) (next ps))
          (recur s2 (next ps))
          s2)))))


(defn <:>
  "Parses p; on failure it pretends it did not consume any input."
  [p]
  (fn [s]
    (let [st (p s)]
      (if (:ok st) st (assoc st :input (:input s) :empty true)))))


(defn many
  "Parses p zero or more times; returns the result(s) in a
   vector. It stops when p fails, but this parser succeeds."
  [p]
  (fn [s]
    (loop [st (p s) vs [] e true]
      (if (and (:ok st) (not (:empty st)))
        (recur (p st) (conj vs (:value st)) (and e (:empty st)))
        (if (:empty st)
          (assoc st :value vs :ok true :empty e :error nil)
          st)))))


(defn many0
  "Like (many) but it won't set the state to :empty. Use instead of
   (many) if it comes last to avoid overriding non-empty parsing."
  [p] (>> (many p) clear-empty))


(defn many1
  "Parses p one or more times and returns the result(s) in a
   vector. It stops when p fails, but this parser succeeds."
  [p] (>>= p (fn [x] (>>= (many p) (fn [y] (return (reduce conj [x] y)))))))


(defn optional
  "Succeeds if p succeeds or if p fails without consuming input."
  [p]
  (fn [s]
    (let [st (p s)]
      (if (or (:ok st) (:empty st)) (reply (:value st) st) st))))


(defn option
  "Applies p; if it fails without consuming input, it returns a
   parser state record with the :value x as default."
  [x p]
  (fn [s]
    (let [st (p s)]
      (if (failed-empty? st)
        (reply x s)
        st))))


(defn skip
  "Applies one or more parsers and skips the result. That is, it
   returns a parser state record with a :value nil."
  ([p]
   (>> p (return nil)))
  ([p q]
   (>>= p (fn [_] (skip q))))
  ([p q & more]
   (reduce skip (list* p q more))))


(defn skip-many
  "Parses p zero or more times and skips the results. This is
   like skip but it can apply p zero, one, or many times."
  [p]
  (fn [s]
    (loop [st (p s) e true]
      (if (and (:ok st) (not (:empty st)))
        (recur (p st) (and e (:empty st)))
        (if (:empty st)
          (assoc st :value nil :ok true :empty e :error nil)
          st)))))


(defn skip-many1
  "Parses p one or more times and skips the results."
  [p] (>> p (skip-many p)))


(defn sep-by1
  "Parses p one or more times while parsing sep in between;
   collects the results of p in a vector."
  [sep p] (>>= p (fn [x] (>>= (many (>> sep p)) (fn [y] (return (reduce conj [x] y)))))))


(defn sep-by
  "Parses p zero or more times while parsing sep in between;
   collects the results of p in a vector."
  [sep p] (<|> (sep-by1 sep p) (return [])))


(defn end-by
  "Parses p zero or more times, separated and ended by applications
   of sep; returns the results of p in a vector."
  [sep p] (many (<< p sep)))


(defn end-by1
  "Parses p one or more times, separated and ended by applications
   of sep; returns the results of p in a vector."
  [sep p] (many1 (<< p sep)))


(declare sep-end-by)

(defn sep-end-by1
  "Parses p one or more times separated, and optionally ended by sep;
   collects the results in a vector."
  [sep p]
  (>>= p (fn [x]
           (<|> (>>= (>> sep (sep-end-by sep p)) (fn [y] (return (reduce conj [x] y))))
                (return [x])))))


(defn sep-end-by
  "Parses p zero or more times separated, and optionally ended by sep;
   collects the results in a vector."
  [sep p] (<|> (sep-end-by1 sep p) (return [])))


(defn between
  "Applies open, p, close; returns the value of p."
  ([delim p] (between delim delim p))
  ([open close p] (<< (>> open p) close)))


(defn times
  "Applies p n times; collects the results in a vector."
  [n p]
  (if (pos? n)
    (apply <*> (repeat n p))
    (return [])))


(defn look-ahead
  "Applies p and returns the result; it consumes no input."
  [p]
  (fn [s]
    (let [st (p s)]
      (assoc s :value (:value st)))))


(defn predict
  "Applies p; if it succeeds it consumes no input."
  [p]
  (fn [s]
    (let [st (p s)]
      (if (not (or (:ok st) (:empty st)))
        st
        (assoc st :input (:input s))))))


(defn not-followed-by
  "Succeeds only if p fails; consumes no input."
  [p]
  (<:> (<|> (>>= (<:> p) (fn [x] (partial unexpected-input x)))
            (return nil))))


(defn many-till
  "Parses zero or more p while trying end, until end succeeds.
   Returns the results in a vector."
  [p end]
  (letfn [(scan [] (<|> (>> end (return []))
                        (>>= p (fn [x] (>>= (scan) (fn [y] (return (reduce conj [x] y))))))))]
    (scan)))


(defn <+>
  "Applies one or more parsers stopping at the first failure.
   Flattens the result and converts it to a string."
  [p & more]
  (<$> (comp join flatten) (apply <*> p more)))


(defn search
  "Applies a parser p, traversing the input as necessary,
   until it succeeds or it reaches the end of input."
  [p]
  (fn [s]
    (let [s2 (p s)]
      (if (or (:ok s2) (empty? (:input s2)))
        s2
        (recur (assoc s2 :input (rest (:input s2)) :error nil))))))


;; +-------------------------------------------------------------+
;; |                     Primitive parsers.                      |
;; +-------------------------------------------------------------+


(def any-char
  "Succeeds with any character."
  (satisfy (constantly true)))


(def letter
  "Parses a letter."
  (<?> (satisfy char/is-letter)
       (di18n :letter)))


(def lower
  "Parses a lower-case letter."
  (<?> (satisfy char/is-lower-case)
       (di18n :lower)))


(def upper
  "Parses an upper-case letter."
  (<?> (satisfy char/is-upper-case)
       (di18n :upper)))


(def white-space
  "Parses a whitespace character."
  (<?> (satisfy char/is-white-space)
       (di18n :whitespace)))


(def space
  "Parses the space character."
  (<?> (satisfy char/is-space)
       (di18n :space)))


(def tab
  "Parses the tab character."
  (<?> (satisfy char/is-tab)
       (di18n :tab)))


(def digit
  "Parses a digit."
  (<?> (satisfy char/is-digit)
       (di18n :digit)))


(def hex-digit
  "Parses a hexadecimal digit."
  (let [hex (set "0123456789abcdefABCDEF")]
    (<?> (satisfy (fn [c] (hex c)))
         (di18n :hex-digit))))


(def oct-digit
  "Parses an octal digit."
  (let [oct (set "01234567")]
    (<?> (satisfy (fn [c] (oct c)))
         (di18n :oct-digit))))


(def alpha-num
  "Parses a letter or digit."
  (<?> (satisfy char/is-letter-or-digit)
       (di18n :alpha-num)))


#?(:clj  (defn sym*
           "Parses a single symbol x (a character)."
           [^Character x]
           (<?> (satisfy (fn [^Character c] (.equals c x)))
                (with-out-str (pr x))))
   :cljs (defn sym*
           "Parses a single symbol x (a character)."
           [x]
           (<?> (satisfy (fn [c] (= c x)))
                (with-out-str (pr x)))))

#?(:clj  (defn sym-
           "Parses a single symbol x (a character); not case-sensitive."
           [^Character x]
           (<?> (>> (satisfy (fn [^Character c]
                               (= (Character/toLowerCase x) (Character/toLowerCase c))))
                    (return x))
                (with-out-str (pr x))))
   :cljs (defn sym-
           "Parses a single symbol x (a character); not case-sensitive."
           [x]
           (<?> (>> (satisfy (fn [c]
                               (= (str/lower-case x) (str/lower-case c))))
                    (return x))
                (with-out-str (pr x)))))



(defn token*
  "Parses a specific string, not necessarily delimited. If more
   than one are given it will try each choice in turn."
  ([xs]
   (<?> (fn [s]
          (let [st ((reduce >> (map sym* xs)) s)]
            (if (:ok st)
              (assoc st :value xs)
              (let [in (:input s)]
                (if (seq in)
                  (unexpected (join (take (count xs) in)) s)
                  (assoc s :value nil :ok false :empty true :error (:error st)))))))
        (str xs)))
  ([xs & more]
   (apply <|> (map token* (cons xs more)))))


(defn token-
  "Parses a specific string, not necessarily delimited; not
   case-sensitive. If more than one are given it will try
   each choice in turn."
  ([xs]
   (<?> (fn [s]
          (let [st ((reduce >> (map sym- xs)) s)]
            (if (:ok st)
              (assoc st :value xs)
              (let [in (:input s)]
                (if (seq in)
                  (unexpected (join (take (count xs) in)) s)
                  (assoc s :value nil :ok false :empty true :error (:error st)))))))
        (str xs)))
  ([xs & more]
   (apply <|> (map token- (cons xs more)))))


(defn word*
  "Parses a specific string, delimited by letter. If more than
   one are given it will try each choice in turn."
  ([letter cs]
   (<:> (<< (token* cs)
            (<?> (not-followed-by letter) (str (i18n :end-of) cs)))))
  ([letter cs & more]
   (apply <|> (map #(word* letter %) (cons cs more)))))


(defn word-
  "Parses a specific string, delimited by letter; not case-sensitive.
   If more than one are given it will try each choice in turn."
  ([letter cs]
   (<:> (<< (token- cs)
            (<?> (not-followed-by letter) (str (i18n :end-of) cs)))))
  ([letter cs & more]
   (apply <|> (map #(word- letter %) (cons cs more)))))



(defn one-of*
  "Succeeds if the next character is in the supplied string."
  [cs] (satisfy #(str/index-of cs %)))

(defn none-of*
  "Succeeds if the next character is not in the supplied string."
  [cs] (satisfy #(nil? (str/index-of cs %))))


(def new-line*
  "Succeeds on a new line."
  (>> (optional (sym* \return))
      (<?> (sym* \newline) (di18n :new-line))))


(def eof
  "Succeeds on end of input."
  (<?> (not-followed-by any-char) (di18n :eof)))


(defn skip-ws
  "Skips whitespaces before parsing p."
  [p] (>> (many white-space) p))


(defn field*
  "Parses an unquoted text field terminated by any character in cs."
  [cs] (<+> (many (none-of* cs))))


(defn split-on
  "Splits a string on one of the given characters and whitespace.
   Removes empty strings from the result."
  [cs]
  (<$> (partial reduce (conjp empty?) [])
       (sep-by (skip (one-of* cs) (many white-space)) (field* cs))))


(def split
  "Splits a string on whitespace."
  (split-on " \t\f\r\n"))


(def mark
  "Succeeds with a punctuation mark."
  (one-of* "~!@#$%^&*()_-=+[]{}\\|;:<>,./?"))


;; +-------------------------------------------------------------+
;; |                      Numeric parsers.                       |
;; +-------------------------------------------------------------+


(defn- rmvz
  "Removes leading zeroes from a string."
  [cs]
  (let [s (join (drop-while #(= % \0) cs))]
    (if (empty? s) "0" s)))


(def dec-num
  "Parses a decimal integer delimited by any character that
   is not a decimal digit."
  (<?> (>>= (<+> (many1 digit))
            (fn [x] (return #?(:clj  (read-string (rmvz x))
                               :cljs (js/eval (rmvz x))))))
       (di18n :dec-lit)))


(def oct-num
  "Parses an octal integer delimited by any character that
   is not an octal digit."
  (<?> (>>= (<+> (many1 oct-digit))
            (fn [x] (return #?(:clj  (read-string (str "0" x))
                               :cljs (js/eval (str "0" x))))))
       (di18n :oct-lit)))


(def hex-num
  "Parses a hex integer delimited by any character that
   is not a hex digit."
  (<?> (>>= (<+> (many1 hex-digit))
            (fn [x] (return #?(:clj  (read-string (str "0x" x))
                               :cljs (js/eval (str "0x" x))))))
       (di18n :hex-lit)))


(def float-num
  "Parses a simple fractional number without an exponent.
   It is delimited by any character that is not a decimal
   digit. It cannot start with a period; the first period
   found must be followed by at least one digit."
  (<?> (>>= (<+> (many1 digit)
                 (option ".0" (<*> (sym* \.) (many1 digit))))
            (fn [x] (return #?(:clj  (read-string x)
                               :cljs (js/eval x)))))
       (di18n :float-lit)))


;; +-------------------------------------------------------------+
;; |                       State parsers.                        |
;; +-------------------------------------------------------------+


(defn get-state
  "Get the user state from the parser state record."
  [s] (assoc s :value (:user s) :ok true :empty true :error nil))


(defn put-state
  "Put u as the new value for user state in the parser state record."
  [u] (fn [s] (assoc s :ok true :empty true :user u :error nil)))


(defn modify-state
  "Modify the user state with the result of f, which takes the old
   user state plus any additional arguments."
  [f & more]
  (fn [s]
    (let [u (apply f (:user s) more)]
      (assoc s :ok true :empty true :user u :error nil))))


(defn get-input
  "Gets the input stream from a parser state."
  [s] (assoc s :value (:input s) :ok true :empty true :error nil))


(defn set-input
  "Sets the input stream in a parser state."
  [in] (fn [s] (assoc s :input (seq in) :ok true :empty true :error nil)))


(defn get-position
  "Gets the position in the input stream of a parser state."
  [s] (assoc s :value (:pos s) :ok true :empty true :error nil))


(defn set-position
  "Sets the position in the input stream of a parser state."
  [pos] (fn [s] (assoc s :pos pos :ok true :empty true :error nil)))


;; +-------------------------------------------------------------+
;; |                     Parser executors.                       |
;; +-------------------------------------------------------------+


(defn parse
  "Parses a character sequence; takes an optional label and a user
   state initial value, which default to nil. Returns a PState record.

   cs    A seqable object; parse calls (seq) on this value.
   src   Identifies the source of the text, e.g. a filename.
   us    Initializes a field that is maintained by client code."
  ([p cs] (parse p cs nil nil))
  ([p cs src] (parse p cs src nil))
  ([p cs src us] (p (->PState (seq cs) (make-pos src) nil true true us nil))))


(defn value
  "Calls (parse) on the arguments and returns the actual parsed
   value, not the PState record."
  ([p cs] (value p cs nil nil))
  ([p cs src] (value p cs src nil))
  ([p cs src us] (:value (parse p cs src us))))


(defn print-error
  "Prints error messages in a PState record."
  [s]
  (let [err (:error s)
        pos (:pos err)
        src (let [l (:src pos)] (if (empty? l) "" (str l " ")))
        ln  (:line pos)
        col (:col pos)]
    (print (fmt :err-pos src ln col))
    (println (get-msg-str err))))


(defn run
  "For testing parsers, e.g. at the REPL. Calls (parse) on the
   arguments and prints the result. If p succeeds it prints the
   parsed value; if it fails it prints any error messages."
  ([p cs] (run p cs nil nil))
  ([p cs src] (run p cs src nil))
  ([p cs src us]
   (let [s (parse p cs src us)]
     (if (:ok s)
       (pprint (:value s))
       (print-error s))
     (if-let [us (:user s)]
       (pprint us)))))


(defn run*
  "For testing parsers, e.g. at the REPL. Works like (run) but
   on success it pretty-prints the resulting parser state."
  ([p cs] (run* p cs nil nil))
  ([p cs src] (run* p cs src nil))
  ([p cs src us] (pprint (parse p cs src us))))


#?(:clj (defn parse-file
          "Parses a file; takes an optional encoding and user state,
           which default to utf-8 and nil. Returns a PState record."
          ([p f] (parse-file p f "UTF-8" nil))
          ([p f en] (parse-file p f en nil))
          ([p f en us] (parse p (slurp f :encoding en) f us))))


#?(:clj (defn runf
          "For testing, e.g. at the REPL, with input from files.
           Prints the results."
          ([p f] (runf p f "UTF-8" nil))
          ([p f en] (runf p f en nil))
          ([p f en us] (run p (slurp f :encoding en) f us))))


#?(:clj (defn runf*
          "For testing, e.g. at the REPL, with input from files.
           Pretty-prints the results."
          ([p f] (runf* p f "UTF-8" nil))
          ([p f en] (runf* p f en nil))
          ([p f en us] (pprint (parse-file p f en us)))))


;; +-------------------------------------------------------------+
;; | Performance tweak.                                          |
;; | This code removes error-handling to just pass or fail. It's |
;; | intended for big data files assumed to have correct syntax. |
;; +-------------------------------------------------------------+


(defn- char-pos-x [x _] x)
(defn- str-pos-x [x _] x)
(defn- merge-err-x [_ _] nil)
(defn- set-ex-x [_ x] x)


(defn parse-data
  "Works like (parse) but with error diagnostics disabled for
   better performance. It's intended for data that can be
   assumed to be correct or its diagnosis postponed."
  ([p cs] (parse-data p cs nil nil))
  ([p cs src] (parse-data p cs src nil))
  ([p cs src us]
   (binding [char-pos  char-pos-x
             str-pos   str-pos-x
             merge-err merge-err-x
             set-ex    set-ex-x]
     (parse p cs src us))))


#?(:clj
   (defn parse-data-file
     "Works like (parse-file) but with error diagnostics disabled for
      better performance. It's intended for data files that can be
      assumed to be correct or its diagnosis postponed."
     ([p f] (parse-data-file p f "UTF-8" nil))
     ([p f en] (parse-data-file p f en nil))
     ([p f en us]
      (binding [char-pos  char-pos-x
                str-pos   str-pos-x
                merge-err merge-err-x
                set-ex    set-ex-x]
        (parse-file p f en us)))))
