(defproject org.blancas/kern "1.0.0"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :profiles
    {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
     :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
     :1.5 {:dependencies [[org.clojure/clojure "1.5.0"]]}
     :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
     :dev {:resource-paths ["src/main/resources" "src/test/resources"]
           :dependencies [[org.clojure/clojure "1.6.0"]
                          [org.clojure/tools.macro "0.1.2"]
                          [org.clojure/tools.trace "0.7.8"]
                          [org.clojure/tools.nrepl "0.2.3"]
                          [midje "1.6.3" :exclusions [org.clojure/clojure]]
                          [criterium "0.4.3"]
                          [jline "1.0"]]
           :plugins [[codox "0.6.8"][lein-midje "3.1.3"]]
           :codox {:sources ["src/main/clojure"] :output-dir "codox"}}})
