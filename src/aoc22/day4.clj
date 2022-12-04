(ns aoc22.day4
  (:gen-class)
  (:require [clojure.string :as str]))

(defn full-overlap [assignment]
  (and
     (<= (get-in assignment [0 0]) (get-in assignment [1 0]))
     (>= (get-in assignment [0 1]) (get-in assignment[1 1]))))

(defn parse-assignment [assignment-string]
  (->>
   (str/split assignment-string #",")
   (map #(str/split % #"-"))
   (map #(vec (map read-string %)))
   (sort-by #(- (get % 0) (get % 1)))
   (vec)))

(defn -main []
  (->>
   (slurp "resources/day4.txt")
   (str/split-lines)
   (map parse-assignment)
   (filter full-overlap)
   (count)
   ))
