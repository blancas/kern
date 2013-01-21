(defproject org.blancas/kern "0.5.0"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :plugins [[codox "0.6.4"]]
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :source-paths ["src/main/clojure" "src/main/resources"]
  :codox {:sources ["src/main/clojure"] :output-dir "codox"}
  :jvm-opts ["-Dfile.encoding=UTF-8"])
