;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Support for a simple i18n scheme."
      :author "Armando Blancas"}
  blancas.kern.i18n)


(def ^:private default
  {:comma       ", "
   :eof         "end of input"
   :letter      "letter"
   :lower       "lowercase letter"
   :upper       "uppercase letter"
   :whitespace  "whitespace"
   :space       "space"
   :new-line    "new line"
   :tab         "tab"
   :digit       "digit"
   :hex-digit   "hexadecimal digit"
   :oct-digit   "octal digit"
   :alpha-num   "letter or digit"
   :end-comment "end of comment"
   :char-lit    "character literal"
   :end-char    "end of character literal"
   :esc-code-b  "escaped code: b, t, n, f, r, ', \\"
   :esc-code-c  "escaped code: b, t, n, f, r, ', \\, ?, a, v, 0, ooo, uhhhh, xhh"
   :esc-code-j  "escaped code: b, t, n, f, r, ', \\, ooo, hhhh"
   :esc-code-h  "escaped code: b, t, n, f, r, ', \\, ?, a, v, 0, nnn, onnn, xnnnn"
   :string-lit  "string literal"
   :end-string  "end of string literal"
   :end-of      "end of "
   :dec-lit     "decimal literal"
   :oct-lit     "octal literal"
   :hex-lit     "hex literal"
   :float-lit   "floating-point literal"})

(def default-fmt
  {:unexpected #(str "unexpected " %)
   :expecting  #(str "expecting " %)
   :or         #(str " or " %)
   :err-pos    #(str %1 " line " %2 " column " %3 "\n")
   :reserved   #(str % " is a reserved name")})


(def ^:private text (atom default))

(def ^:private fmt-text (atom default-fmt))


(defn i18n-merge
  "Merges m into the text map for customization."
  [m] (swap! text merge m))


(defn i18n
  "Gets or sets the value for the supplied key."
  ([k] (k (deref text)))
  ([k v] (swap! text assoc k v)))


(defn fmt
  "Formats a string with a key and more arguments."
  [k & more]
  (apply (k @fmt-text) more))


(defn di18n
  "Returns a Delay instance with the value for the supplied key.
   Useful in (def)'ed expressions that evaluate too soon."
  [k] (delay (k (deref text))))


(defn dfmt
  "Returns a Delay instance with a string formatted with a key and more
   arguments. Useful in (def)'ed expressions that evaluate too soon."
  [k & more]
  (delay (apply fmt k more)))
