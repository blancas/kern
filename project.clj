(defproject org.blancas/kern "0.6.1"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :profiles
    {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
     :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
     :1.5 {:dependencies [[org.clojure/clojure "1.5.0"]]}
     :dev {:resource-paths ["src/main/resources" "src/test/resources"]
           :dependencies [[midje "1.5.0" :exclusions [org.clojure/clojure]]]
           :plugins [[codox "0.6.4"]]
           :codox {:sources ["src/main/clojure"] :output-dir "codox"}}})
