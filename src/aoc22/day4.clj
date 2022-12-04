(ns aoc22.day4
  (:gen-class)
  (:require [clojure.string :as str]))

(defn numberInRange [number range] 
  (and (>= number (range 0)) (<= number (range 1))))

(defn full-overlap [assignment]
  (every? #(numberInRange % (assignment 0)) (assignment 1)))

(defn any-overlap [assignment]
  (some #(numberInRange % (assignment 0)) (assignment 1)))

(defn parse-assignment [assignment-string]
  (->>
   (str/split assignment-string #",")
   (map #(str/split % #"-")) 
   (map #(->> % (map read-string) (vec)))
   (sort-by #(- (get % 0) (get % 1)))
   (vec)))

(defn -main []
  (->>
   (slurp "resources/day4.txt")
   (str/split-lines)
   (map parse-assignment)
   (filter any-overlap) ;(filter full-overlap) for task1
   (count)))
