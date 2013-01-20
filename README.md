# Kern

Kern is a library of parser combinators for Clojure. It is useful for 
implementing recursive-descent parsers based on predictive LL(1) grammars 
with on-demand, unlimited look-ahead. The inspiration for Kern comes from 
Parsec, a Haskell library written by Daan Leijen,  and from work by Graham 
Hutton, Erik Meijer, and William Burge.

## Features

* Purely functional implementation based on a custom state monad.
* Lexer support for various language styles: C, Java, Haskell, Shell.
* Support for parsing and evaluating expressions.
* Ability to produce accurate and detailed error messages.
* A simple, dynamic i18n scheme for error messages.
* Access to the parser's internal state from client code.
* Sample parsers in `src/main/resources`.

## Setup

Leiningen:

```clojure
[org.blancas/kern "0.5.0"]
```

Maven:

```xml
<dependency>
  <groupId>org.blancas</groupId>
  <artifactId>kern</artifactId>
  <version>0.5.0</version>
</dependency>
```

## Sample Usage

Parsing JSON data.

    pair    ::=  string ':' json
    array   ::=  '[' (json (',' json)*)* ']'
    object  ::=  '{' (pair (',' pair)*)* '}'
    json    ::=  string | number | object | array | true | false | null
```clojure
(use 'blancas.kern.core
     'blancas.kern.lexer.basic)

(declare json)

(def pair (bind [f string-lit _ colon v (fwd json)]
            (return [f v])))

(def array (brackets
             (bind [elements (comma-sep (fwd json))]
               (return (vec elements)))))
  
(def object (braces
              (bind [fields (comma-sep pair)]
                (return (apply hash-map (reduce concat [] fields))))))

(def json (<|> string-lit dec-lit float-lit object array bool-lit nil-lit))
```
Evaluate the `json` parser:

```clojure
(run json "{\"fst\": \"Joe\", \"lst\": \"Hacks\",\"id\":1122}")
;; {"fst" "Joe", "lst" "Hacks", "id" 1122}
(run json "{\"id\":1122,\"scores\":[400,125,999],\"top\":true}")
;; {"scores" [400 125 999], "top" true, "id" 1122}
```

## Documentation

Kern is documented in the [Wiki](https://github.com/blancas/kern/wiki).

To generate the internal documentation (in the `codox` directory):

    lein doc

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
