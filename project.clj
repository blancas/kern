(defproject ru.prepor/kern "1.2.0-SNAPSHOT"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.854"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :deploy-repositories [["releases" :clojars]]
  :cljsbuild {:builds {:dev  {:source-paths ["src/main/clojure" "src/test/clojure"]
                              :compiler     {:main          blancas.kern.core
                                             :optimizations :none
                                             :asset-path "js/out"
                                             :output-to "resources/public/js/dev.js"
                                             :output-dir "resources/public/js/out"}
                              :figwheel true}
                       :test {:source-paths ["src/main/clojure" "src/test/clojure"]
                              :compiler     {:output-to     "out/testable.js"
                                             :main          blancas.kern.runnner
                                             :optimizations :none}}}}
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
                          [bultitude "0.2.6"] [org.clojure/core.match "0.3.0-alpha5"]
                          [criterium "0.4.4"]
                          [jline "1.0"]
                          [doo "0.1.7"]]
           :plugins [[codox "0.9.4"]
                     [lein-doo "0.1.7"]
                     [lein-figwheel "0.5.12"]]
           :codox {:source-paths ["src/main/clojure"] :output-path "codox"}
           :doo {:build "test"
                 :alias {:browsers [:chrome :firefox]
                         :all [:browsers :headless]}}}})
