(ns blancas.kern.char)

(defn is-letter [^Character c]
  (Character/isLetter c))


(defn is-lower-case [^Character c]
  (Character/isLowerCase c))


(defn is-upper-case [^Character c]
  (Character/isUpperCase c))


(defn is-white-space [^Character c]
  (Character/isWhitespace c))


(defn is-space [^Character c]
  (.equals c \space))


(defn is-space [^Character c]
  (.equals c \space))


(defn is-tab [^Character c]
  (.equals c \tab))


(defn is-digit [^Character c]
  (Character/isDigit c))

(defn is-letter-or-digit [^Character c]
  (Character/isLetterOrDigit c))

(defn upper-case [^Character c]
  (Character/toUpperCase c))