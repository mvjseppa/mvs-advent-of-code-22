(ns aoc22.day4
  (:gen-class)
  (:require [clojure.string :as str]))

(defn full-overlap [assignment]
  (and
   (<= (get-in assignment [0 0]) (get-in assignment [1 0]))
   (>= (get-in assignment [0 1]) (get-in assignment [1 1]))))

(defn any-overlap [assignment]
  (not (or
   (every? #(< % (get-in assignment [0 0])) (assignment 1))
   (every? #(> % (get-in assignment [0 1])) (assignment 1))))) 

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
