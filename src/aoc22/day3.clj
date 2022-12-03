(ns aoc22.day3
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set]))

(defn item-value [item]
  (if (Character/isUpperCase item)
    (+ 27 (- (int item) (int \A)))
    (+ 1  (- (int item) (int \a)))))


(defn common-item [rucksack]
  (first
   (apply clojure.set/intersection
          (map set
               (split-at
                (/ (count rucksack) 2) rucksack)))))

(defn -main [] (->>
                (slurp "resources/day3.txt")
                (str/split-lines)
                (map common-item)
                (map item-value)
                (reduce +)))

