(ns blancas.kern.runnner
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-all-tests doo-tests]]
            [blancas.kern.test-core]
            [blancas.kern.test-lexer]
            [blancas.kern.test-lexer-c]
            [blancas.kern.test-lexer-haskell]
            [blancas.kern.test-lexer-java]
            [blancas.kern.test-lexer-shell]))

(enable-console-print!)

(doo-tests 'blancas.kern.test-core
           'blancas.kern.test-lexer
           'blancas.kern.test-lexer-c
           'blancas.kern.test-lexer-haskell
           'blancas.kern.test-lexer-java
           'blancas.kern.test-lexer-shell)
