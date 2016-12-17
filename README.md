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

The library is intended for parsing text of all kinds. In addition, there
is support for parsing programming languages and data formats with language-like
notations like JSON and EDN. The lexer combinators may be configured
for various language features like handling of whitespace, block and line comments,
case sensitivity, and patterns for identifiers, among others.


## Parser Combinators

A parser is a high-order function whose job is to match an input string against 
a specific pattern. A combinator is a parsing function whose 
pattern is determined by parsers or by other combinators for the purposes 
of providing repetition, choice, filtering, or to enforce a certain order in the input.
We also use the term *parser* in its more general sense, as a logical module of 
parsing functions for a particular task.

The key feature of this technique is the ability to devise a composition and
sequencing of combinators for parsing a grammar, and to do so in such a way
that the resulting code resembles the structure of said grammar. Furthermore,
the resulting parser may itsef be composed, like any other, custom or predefined,
 to form more elaborate combinators.

Thus parser combinators offer means of abstraction and composition
that result in a powerful and pleasant way to code parsing modules. Kern provides
a rich set of parsers for higher productivity and not having to start from scratch.
The next section illustrates the above by defining a custom parser for JSON data. 

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

(def pair (bind [f string-lit _ colon v json]
            (return [f v])))

(def array (brackets (comma-sep (fwd json))))

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

## Performance

Kern's design isn't well-suited for achieving very high performance. It is based
on pure functions and the systematic composition of its core parsers. My main
goals are high productivity and usability. I'll make, however, every effort
to improve the efficiency of the library.

Just to give a ballpark figure for performance at the REPL, on my MacBook Pro, 
without any JVM warm up, Kern will parse 350K worth of JSON data per second 
using the above definitions, with variations of some 10% either way. In contrast, 
[data.json](https://github.com/clojure/data.json) will parse 4MB in a quarter
of a second. (I realize times can vary a lot, but after a few runs they
converge somewhat; YMMV.)

I've written Kern as my preferred way of working on domain-specific languages,
stand-alone and for program extensions. For these jobs I've found its performance
quite satisfactory, with the big pay-off being the boost in productivity.

## Setup

Leiningen:

```clojure
[org.blancas/kern "1.1.0"]
```

Maven:

```xml
<dependency>
  <groupId>org.blancas</groupId>
  <artifactId>kern</artifactId>
  <version>1.1.0</version>
</dependency>
```

### Changes for release 1.1.0:

* Fixed issue #13: times failed to parse once.
* Fixed <+> when using a single parser.
* Reduced the arity of <*> to 1 allowing for 1+ parsers to be applied, adding this missing flexibility.

## Documentation

Browse the whole [change log](https://github.com/blancas/kern/wiki/Change-Log).

Kern is documented in the [Wiki](https://github.com/blancas/kern/wiki).

Browse the Codox [Kern v1.1.0 API](http://blancas.github.com/kern).

## YourKit

YourKit is kindly supporting open source projects with its full-featured Java
Profiler.

YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
