(defproject sicp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC BY-SA 4.0"
            :url "https://creativecommons.org/licenses/by-sa/4.0/legalcode"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :plugins [[lein-cljfmt "0.9.2"]]
  :main ^:skip-aot sicp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
