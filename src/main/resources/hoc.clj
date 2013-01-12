(ns ^{:author "Armando Blancas"
      :doc "A HOC interpreter, as specified in:

Hoc - An Interactive Language For Floating Point Arithmetic

The UNIX Programming Environment
By Brian Kernigham, Rob Pike
Prentice-Hall, 1983"}
  hoc
  (:use [blancas.kern core expr]
        [blancas.kern.lexer.shell-style]
        [clojure.string :only (upper-case)]))

;; To try:
(comment
(load "hoc")
(ns hoc)
(hf "src/main/resources/ack.hoc")
(hf "src/main/resources/fact.hoc")
(hf "src/main/resources/stirling.hoc")
)

;;  +--------------------------------------------------------+
;;  |                         LEXER                          |
;;  +--------------------------------------------------------+

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

(def pow-op (>> (sym \^) (return #(Math/pow %1 %2))))

(def uni-op
  (bind [op (one-of "!-")]
    (return ({\! not \- -} op))))

(def mul-op
  (bind [op (one-of "*/%")] 
    (return ({\* * \/ / \% mod} op))))

(def add-op
  (bind [op (one-of "+-")] 
    (return ({\+ + \- -} op))))

(def rel-op
  (bind [op (token "==" "!=" ">=" "<=" ">" "<")]
    (return ({"==" = "!=" not= ">=" >= "<=" <= ">" > "<" <} op))))

(def and-op (>> (token "&&") (return #(and %1 %2))))
(def or-op  (>> (token "||") (return #(or %1 %2))))
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
  (bind [_ (sym \{) _ new-line ss (many (<< (fwd stmt) ending)) _ (sym \})]
    (return {:token :BLOCK :seq ss})))

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

(def void-stmt (look-ahead new-line))

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
