(ns blancas.kern.char
  (:require [clojure.string :as str]))

(defn is-letter [c]
  (re-find #"^[a-zA-Z]$" c))


(defn is-lower-case [c]
  (re-find #"^[a-z]$" c))


(defn is-upper-case [c]
  (re-find #"^[A-Z]$" c))


(defn is-white-space [c]
  (re-find #"^\s$" c))


(defn is-space [c]
  (= c \space))


(defn is-tab [c]
  (= c \tab))


(defn is-digit [c]
  (re-find #"^[0-9]$" c))


(defn is-letter-or-digit [c]
  (re-find #"^[a-zA-Z0-9]$" c))

(defn upper-case [c] (str/upper-case c))