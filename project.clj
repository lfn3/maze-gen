(defproject maze-gen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha20"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [org.clojure/test.check "0.9.0"]
                 [rhizome "0.2.9"]]
  :main ^:skip-aot maze-gen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
