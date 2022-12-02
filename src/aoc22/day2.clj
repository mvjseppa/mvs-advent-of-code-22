(ns aoc22.day2
  (:gen-class) 
  (:require [clojure.string :as str]))

(def losing-response 
  {"A" "C"
   "B" "A"
   "C" "B"})

(def shape-score
  {"A" 1
   "B" 2
   "C" 3})

(defn decrypted-shape [shape0 shape1] 
  (case shape1 
    "X" (losing-response shape0)
    "Y" shape0
    "Z" (losing-response (losing-response shape0))))

(defn win-score [shape0 shape1]
  (cond
    (= shape0 (losing-response shape1)) 6
    (= shape0 shape1) 3
    :else 0))

(defn round-score [round-string] 
  (let [shapes (str/split round-string #" ")
        shape0 (shapes 0)
        shape1 (decrypted-shape (shapes 0) (shapes 1))]
    (+ 
     (shape-score shape1)
     (win-score shape0 shape1))))

(defn -main [] (->> 
 (slurp "resources/day2.txt")
 (str/split-lines)
 (map round-score)
 (reduce +)))
