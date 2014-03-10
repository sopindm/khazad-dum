(defproject khazad-dum "0.2.0"
  :description "Clojure unit test framework"
  :url ""
  :license {:name "Apache 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :profiles {:dev {:dependencies [[org.clojure/core.match "0.2.1"]]
                   :repl-options {:init (use 'khazad-dum)}}}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [bultitude "0.2.5"]])


