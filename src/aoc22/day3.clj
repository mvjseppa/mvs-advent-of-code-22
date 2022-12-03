(ns aoc22.day3
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set]))

(defn item-value [item]
  (if (Character/isUpperCase item)
    (+ 27 (- (int item) (int \A)))
    (+ 1  (- (int item) (int \a)))))

(defn common-item [rucksack-group]
  (first (apply clojure.set/intersection 
                (map set rucksack-group))))

(defn -main []
  (->>
   (slurp "resources/day3.txt")
   (str/split-lines)
   (partition 3)
   (map common-item)
   (map item-value)
   (reduce +)))
