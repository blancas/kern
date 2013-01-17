(ns ^{:doc "Evaluation of infix expressions."
      :author "Armando Blancas"}
  expr
  (:use [blancas.kern core expr]
        [blancas.kern.lexer.c-style]))

;; To try:
(comment
(load "expr")
(ns expr)
(run expr "3 + 4")
(run expr "2 * (5 + 5)")
(run expr "atan(0.3+0.12345)*3+3*4")
(run expr "sin(0.5) >= sqrt(3)")
)

(declare expr)

(def fun-tbl
  "Definition of built-in functions."
  {"abs"   #(Math/abs %)   "atan" #(Math/atan %)      "cos"  #(Math/cos %)
   "exp"   #(Math/exp %)   "int"  #(Math/round %)     "log"  #(Math/log %)
   "log10" #(Math/log10 %) "sin"  #(Math/sin %)       "sqrt" #(Math/sqrt %)
   "tan"   #(Math/tan %)   "tanh" #(Math/tanh %)      "ceil" #(Math/ceil %)
   "floor" #(Math/floor %) "deg"  #(Math/toDegrees %) "rad"  #(Math/toRadians %)})

(def funcall
"This parser corresponds a syntax rule: FUN := ID (expr)
 It returns the result of applying the primitive function to the
 result of the expression."
  (bind [id identifier ex (parens expr)]
      (return ((fun-tbl id) ex))))

(def factor
  "Evaluates a number (double), a call to a buil-in function, or an
   expression in parens to (for example) change the order of evaluation."
  (<|> float-lit funcall (parens (fwd expr))))

;; Chain calls parse multiple occurrences of a kind of operator
;; working on operands that may be expressions. In this case,
;; uni-op operators have the highest precedence while the and-op
;; operator has the lowest.

(def unary (prefix1 factor uni-op))  ;; -(10), !(3>0)
(def power (chainr1 unary  pow-op))  ;; 2^32
(def term  (chainl1 power  mul-op))  ;; 3 * 34 * ...
(def sum   (chainl1 term   add-op))  ;; 5 + 2*3 + ...
(def relex (chainl1 sum    rel-op))  ;; sin(0.5) > 0
(def orex  (chainl1 relex  or-op))   ;; sqrt(10) > 0 || tan(0) == 1 || ...
(def expr  (chainl1 orex   and-op))  ;; sqrt(10) > 0 && tan(0) == 1 || sin(0) > 0 & ...
