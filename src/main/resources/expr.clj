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

;; The following functions parse an operator and produce the
;; function that corresponds to that operator.

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

(def or-op (>> (token "||") (return #(or %1 %2))))

(def fun-tbl
  "Definition of built-in functions."
  {"abs"   #(Math/abs %)   "atan" #(Math/atan %)      "cos"  #(Math/cos %)
   "exp"   #(Math/exp %)   "int"  #(Math/round %)     "log"  #(Math/log %)
   "log10" #(Math/log10 %) "sin"  #(Math/sin %)       "sqrt" #(Math/sqrt %)
   "tan"   #(Math/tan %)   "tanh" #(Math/tanh %)      "ceil" #(Math/ceil %)
   "floor" #(Math/floor %) "deg"  #(Math/toDegrees %) "rad" #(Math/toRadians %)})

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

(def unary (prefix1 factor uni-op))  ;; -(10)
(def power (chainr1 unary  pow-op))  ;; 2^32
(def term  (chainl1 power  mul-op))  ;; 3 * 34 * ...
(def sum   (chainl1 term   add-op))  ;; 5 + 2*3 + ...
(def relex (chainl1 sum    rel-op))  ;; sin(0.5) > 0
(def orex  (chainl1 relex  or-op))   ;; sqrt(10) > 0 || tan(0) == 1 || ...
(def expr  (chainl1 orex   and-op))  ;; sqrt(10) > 0 && tan(0) == 1 || sin(0) > 0 & ...
