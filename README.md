# Kern

Kern is a library of parser combinators for Clojure. It is useful for 
implementing recursive-descent parsers based on predictive LL(1) grammars 
with on-demand, unlimited look-ahead.

The main inspiration for Kern comes from Parsec, a Haskell library written 
by Daan Leijen, as well as work by Graham Hutton, Erik Meijer, and William Burge.
The name Kern is a token of appreciation to Brian Kernighan (now at Princeton) 
for his work on programming languages.

Daan Leijen,
[Parsec, a fast combinator parser](http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf)
Graham Hutton and Erik Meijer,
[Monadic Parser Combinators](http://eprints.nottingham.ac.uk/237/1/monparsing.pdf)
William H. Burge,
Recursive Programming Techniques, Addison-Wesley, 1975

## Features

* Purely functional implementation based on a custom state monad.
* A complete set of parser combinators for common tasks.
* Lexer support for various language styles: C, Java, Haskell, Shell.
* Support for arithmetic expressions producing values or syntax nodes.
* Ability to produce accurate and detailed error messages.
* A simple, dynamic i18n scheme for error messages.
* Access to the parser's internal state from client code.
* Storage, modification and retrieval of client data.
* Sample parsers in `src/main/resources`.

## Setup

Leiningen:

```clojure
[org.blancas/kern "0.1.0"]
```

Maven:

```xml
<dependency>
  <groupId>org.blancas</groupId>
  <artifactId>kern</artifactId>
  <version>0.1.0</version>
</dependency>
```

## Sample Usage

Grammar for simple arithmetic expressions:

    expr    ::=  term (add-op term)*
    term    ::=  factor (mul-op factor)*
    factor  ::=  float | ( expr )

```clojure
(use '[blancas.kern core expr]
     '[blancas.kern.lexer.basic])

(declare expr)

(def mul-op (bind [op (one-of "*/%")] 
              (return ({\* * \/ / \% mod} op))))

(def add-op (bind [op (one-of "+-")] 
              (return ({\+ + \- -} op))))

(def factor (<|> float-lit (parens (fwd expr))))
(def term   (chainl1 factor mul-op))
(def expr   (chainl1 term   add-op))
```

Now we evaluate the *expr* combinator with various inputs:

```clojure
(value expr "3 + 4")
;; 7.0
(value expr "2.5 * 4 + 6 / 1.5 - 3")
;; 11.0
(value expr "0.725 * (17 + 11 % 4)")
;; 14.5
```

## Documentation

To generate the documentation in the `codox` directory:

    lein doc

## Building Kern

To build Kern in the `target` directory:

    mvn package

To build Kern and install it the local Maven repository:

    mvn install

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
