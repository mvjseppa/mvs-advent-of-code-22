(defproject mvjs-aoc22 "0.1.0-SNAPSHOT"
  :description "mvjseppa's AoC22 solutions"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot aoc22.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
