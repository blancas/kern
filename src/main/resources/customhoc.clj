;; The HOC interpreter with a customized lexer.
(ns customhoc
  (:use [blancas.kern core expr]
        [clojure.string :only (upper-case)])
  (:require [blancas.kern.lexer :as lex]))

;; To try:
(comment
(load "customhoc")
(ns customhoc)
(hf "src/main/resources/custom-fact.hoc")
)

;;  +--------------------------------------------------------+
;;  |               LEXER WITH CUSTOMIZATIONS                |
;;  +--------------------------------------------------------+

;; To customize the lexer, change 'basic-def' fields as needed.
(def hoc-style
  (assoc lex/basic-def
    :comment-start       "(*"
    :comment-end         "*)"
    :nested-comments     true
    :identifier-letter   (<|> alpha-num (one-of* "_-."))
    :reserved-names      ["while" "if" "else" "read" "print" "return" "fun" "proc"]
    :case-sensitive      false
    :trim-newline        false))

;; Then make the customized parsers.
(def rec (lex/make-parsers hoc-style))

;; For easy access, store the parsers in vars.
(def trim       (:trim       rec))
(def sym        (:sym        rec))
(def new-line   (:new-line   rec))
(def word       (:word       rec))
(def string-lit (:string-lit rec))
(def dec-lit    (:dec-lit    rec))
(def float-lit  (:float-lit  rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def comma-sep  (:comma-sep  rec))

(def identifier (<$> upper-case (:identifier rec)))

(declare expr stmt evalhoc)

(def number
  (bind [d float-lit]
    (return {:token :NUMBER :value d})))

(def string-literal
  (bind [s string-lit]
    (return {:token :STRING :value s})))

(def ident 
  (bind [id identifier]
    (return {:token :IDENT :value id})))

(def argument
  (bind [_ (sym* \$) idx dec-lit]
    (return {:token :ARGMNT :value idx})))

(def rvalue 
  (bind [id identifier es (option :nil (parens (comma-sep expr)))]
    (if (= es :nil)
      (return {:token :IDENT :value id})
      (return {:token :FUNCALL :name id :args es}))))

(def ending (<|> (skip new-line trim) eof))

(def equ-op (>> (sym \=) (return nil)))

;;  +--------------------------------------------------------+
;;  |                        PARSER                          |
;;  +--------------------------------------------------------+

(def factor (<|> (parens (fwd expr)) string-literal rvalue number argument))
(def unary  (prefix1* :UNIOP  factor uni-op))
(def power  (chainr1* :BINOP  unary  pow-op))
(def term   (chainl1* :BINOP  power  mul-op))
(def sum    (chainl1* :BINOP  term   add-op))
(def relex  (chainl1* :BINOP  sum    rel-op))
(def orex   (chainl1* :BINOP  relex  or-op))
(def andex  (chainl1* :BINOP  orex   and-op))
(def expr   (chainr1* :ASSIGN andex  equ-op))

(def block-stmt
  (braces (bind [_ new-line ss (many (<< (fwd stmt) ending))]
            (return {:token :BLOCK :seq ss}))))

(def loop-stmt
  (bind [_ (word "while") e (parens expr) s stmt]
    (return {:token :LOOP :test e :body s})))

(def cond-stmt
  (bind [_ (word "if") e (parens expr) s stmt
	 a (optional (>> (word "else") stmt))]
    (return {:token :COND :test e :body s :alt a})))

(def read-stmt
  (bind [_ (word "read") id (parens identifier)]
    (return {:token :READ :arg id})))

(def print-stmt
  (bind [_ (word "print") e (comma-sep expr)]
    (return {:token :PRINT :args e})))

(def return-stmt
  (bind [_ (word "return") e (optional expr)]
    (return {:token :RETURN :value e})))

(def void-stmt (predict new-line))

(def stmt (<|> block-stmt loop-stmt cond-stmt read-stmt
	       print-stmt return-stmt void-stmt expr))

(def func
  (bind [t (word "func" "proc") i identifier _ (sym \() _ (sym \)) s stmt]
    (return {:token (keyword (upper-case t)) :name i :body s})))

(def parse-hoc (>> trim (many1 (<< (<|> func stmt) ending))))

;;  +--------------------------------------------------------+
;;  |                     INTERPRETER                        |
;;  +--------------------------------------------------------+

(def sym-tbl (atom { "DEG"   57.29577951308232087680
	             "E"      2.71828182845904523536
	             "GAMMA"  0.57721566490153286060
                     "PHI"    1.61803398874989484820
	             "PI"     3.14159265358979323846}))

(def prim-tbl
  {"abs"   #(Math/abs %)   "atan" #(Math/atan %)  "cos"  #(Math/cos %)
   "exp"   #(Math/exp %)   "int"  #(Math/round %) "log"  #(Math/log %)
   "log10" #(Math/log10 %) "sin"  #(Math/sin %)   "sqrt" #(Math/sqrt %)})

(def arg-tbl (atom []))

(defn eval-uniop  [exp] ((:op exp) (evalhoc (:right exp))))
(defn eval-binop  [exp] ((:op exp) (evalhoc (:left exp)) (evalhoc (:right exp))))
(defn eval-lit    [exp] (:value exp))
(defn eval-rvalue [exp] (@sym-tbl (:value exp)))
(defn eval-argmnt [exp] (@arg-tbl (:value exp)))
(defn eval-lvalue [exp] (:value (:left exp)))

(defn eval-assign [exp]
  (let [val (evalhoc (:right exp))]
    (swap! sym-tbl assoc (eval-lvalue exp) val)
    val))

(defn eval-defun [exp]
  (swap! sym-tbl assoc (:name exp) exp)
  nil)

(defn eval-return [exp]
  (swap! arg-tbl assoc 0 (evalhoc (:value exp)))
  (throw (new Exception)))

(defn eval-funcall [{name :name args :args}]
  (if-let [func (prim-tbl name)]
    (func (evalhoc (first args)))
    (let [body (:body (@sym-tbl name))]
      (reset! arg-tbl (vec (map evalhoc (cons nil args))))
      (try
        (evalhoc body) nil
        (catch Throwable t (@arg-tbl 0))))))

(defn eval-loop [{test :test body :body}]
  (loop [doit (evalhoc test)]
    (when doit
      (evalhoc body)
      (recur (evalhoc test)))))

(defn eval-cond [{test :test body :body alt :alt}]
  (if (evalhoc test)
    (evalhoc body)
    (when alt (evalhoc alt))))

(defn eval-block [{lst :seq}]
  (doseq [exp lst] (evalhoc exp)))

(defn eval-read [{arg :arg}]
  (let [scn (java.util.Scanner. System/in)
	str (.nextLine scn)
        val (value (optional float-lit) str)]
    (swap! sym-tbl assoc arg (or val str)))
  nil)

(defn eval-print [{args :args}]
  (let [vals (map evalhoc args)]
    (apply print vals)))

(defn evalhoc [exp]
  (case (:token exp)
    :UNIOP   (eval-uniop   exp)
    :BINOP   (eval-binop   exp)
    :NUMBER  (eval-lit     exp)
    :STRING  (eval-lit     exp)
    :IDENT   (eval-rvalue  exp)
    :ARGMNT  (eval-argmnt  exp)
    :FUNCALL (eval-funcall exp)
    :ASSIGN  (eval-assign  exp)
    :FUNC    (eval-defun   exp)
    :PROC    (eval-defun   exp)
    :LOOP    (eval-loop    exp)
    :COND    (eval-cond    exp)
    :RETURN  (eval-return  exp)
    :BLOCK   (eval-block   exp)
    :READ    (eval-read    exp)
    :PRINT   (eval-print   exp)
    nil      nil))

;;  +--------------------------------------------------------+
;;  |                       INTERFACE                        |
;;  +--------------------------------------------------------+

(defn hoc [cs]
  (let [ast (parse parse-hoc cs)]
    (if (:ok ast)
      (doseq [exp (:value ast)]
        (let [tok (:token exp)
	      val (evalhoc exp)]
	  (when (and val (member? tok [:UNIOP :BINOP :IDENT :FUNCALL]))
	    (println val))))
      (print-error ast))))

(defn hf [cs] (hoc (slurp cs)))
