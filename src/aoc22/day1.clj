(ns aoc22.day1
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main []
  (->>
   (str/split (slurp "resources/day1.txt") #"\n\n")
   (map #(->>
     (str/split % #"\n")
     (map read-string)
     (reduce +)))
   (sort)
   (reverse)
   (take 3)
   (reduce +)))
