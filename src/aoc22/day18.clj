(ns aoc22.day18
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def cubes
  (->> (slurp "resources/day18.txt")
       (str/split-lines)
       (map (fn [line] (read-string (str "[" line "]"))))
       (set)))

(defn neighbors [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]}
  )

(defn free-sides [cube]
  (- 6 (count (set/intersection cubes (neighbors cube)))))

(reduce + (map free-sides cubes))