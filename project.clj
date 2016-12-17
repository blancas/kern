(defproject org.blancas/kern "1.1.0"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :deploy-repositories [["releases" :clojars]]
  :profiles
    {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
     :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
     :1.5 {:dependencies [[org.clojure/clojure "1.5.0"]]}
     :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
     :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
     :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
     :dev {:resource-paths ["src/main/resources" "src/test/resources"]
           :dependencies [[org.clojure/clojure "1.8.0"]
                          [org.clojure/tools.macro "0.1.2"]
                          [org.clojure/tools.trace "0.7.9"]
                          [org.clojure/tools.nrepl "0.2.11"]
                          [midje "1.8.3" :exclusions [org.clojure/clojure]]
                          [criterium "0.4.4"]
                          [jline "1.0"]]
           :plugins [[codox "0.9.4"][lein-midje "3.2"]]
           :codox {:source-paths ["src/main/clojure"] :output-path "codox"}}})
