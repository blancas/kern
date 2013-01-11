(ns ^{:doc "Localization into Spanish."
      :author "Armando Blancas"}
  local
  (:use [blancas.kern core i18n]
        [blancas.kern.lexer.c-style]))

;; To try:
(comment
(load "local")
(ns local)
(i18n-merge spanish)
(run digit "x")
)

(def spanish
     { :unexpected   "no se esperaba %s"
       :expecting    "se esperaba %s"
       :comma        ", "
       :or           " o %s"
       :err-pos      "%slínea %d columna %d\n"
       :eof          "fin del texto"
       :letter       "letra"
       :lower        "letra minúscula"
       :upper        "letra mayúscula"
       :whitespace   "espacio en blanco"
       :space        "espacio"
       :new-line     "línea nueva"
       :tab          "tabulador"
       :digit        "dígito"
       :hex-digit    "dígito hexadecimal"
       :oct-digit    "dígito octal"
       :alpha-num    "letra o dígito"
       :end-comment  "fin del comentario"
       :char-lit     "caracter literal"
       :end-char     "fin de caracter literal"
       :esc-code-b   "código de escape: b, t, n, f, r, ', \\"
       :esc-code-c   "código de escape: b, t, n, f, r, ', \\, ?, a, v, 0, ooo, uhhhh, xhh"
       :esc-code-j   "código de escape: b, t, n, f, r, ', \\, ooo, hhhh"
       :esc-code-h   "código de escape: b, t, n, f, r, ', \\, ?, a, v, 0, nnn, onnn, xnnnn"
       :string-lit   "cadena literal"
       :end-string   "fin de cadena literal"
       :end-of       "fin de "
       :dec-lit      "literal decimal"
       :oct-lit      "literal octal"
       :hex-lit      "literal hexadecimal"
       :float-lit    "literal de punto flotante"
       :reserved     "%s es una palabra reservada"
     })
